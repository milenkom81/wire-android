/**
 * Wire
 * Copyright (C) 2018 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.calling.controllers

import android.Manifest.permission.{CAMERA, RECORD_AUDIO}
import android.content.{Context, DialogInterface, Intent}
import android.net.Uri
import android.os.PowerManager
import android.provider.Settings
import android.support.v7.app.AlertDialog
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.{IConversation, NetworkMode, Verification}
import com.waz.content.GlobalPreferences.AutoAnswerCallPrefKey
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{ConvId, ConversationData, UserId}
import com.waz.permissions.PermissionsService
import com.waz.service.call.CallInfo
import com.waz.service.call.CallInfo.CallState._
import com.waz.service.{AccountsService, GlobalModule, NetworkModeService, ZMessaging}
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.returning
import com.waz.zclient.calling.CallingActivity
import com.waz.zclient.common.controllers.SoundController
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.utils.ContextUtils._
import com.waz.zclient.utils.PhoneUtils.PhoneState
import com.waz.zclient.utils.{DeprecationUtils, PhoneUtils, ViewUtils}
import com.waz.zclient.{Injectable, Injector, R, WireContext}

import scala.concurrent.{Future, Promise}
import scala.util.Success
import scala.util.control.NonFatal

class GlobalCallingController(implicit inj: Injector, cxt: WireContext, eventContext: EventContext) extends Injectable {

  import Threading.Implicits.Ui

  val screenManager          = new ScreenManager
  val soundController        = inject[SoundController]
  val conversationController = inject[ConversationController]
  val networkMode            = inject[NetworkModeService].networkMode
  val permissions            = inject[PermissionsService]
  val accounts               = inject[AccountsService]

  //The zms of the account that's currently active (if any)
  val activeZmsOpt = inject[Signal[Option[ZMessaging]]]

  //the zms of the account that currently has an active call (if any)
  val callingZms = {
    for {
      acc <- inject[GlobalModule].calling.activeAccount
      zms <- acc.fold(Signal.const(Option.empty[ZMessaging]))(id => Signal.future(ZMessaging.currentAccounts.getZms(id)))
    } yield zms
  }

  val currentCallOpt: Signal[Option[CallInfo]] = callingZms.flatMap {
    case Some(z) => z.calling.currentCall
    case _ => Signal.const(None)
  }

  val callConvIdOpt     = currentCallOpt.map(_.map(_.convId))
  val isCallActive      = currentCallOpt.map(_.isDefined)
  val callStateOpt      = currentCallOpt.map(_.flatMap(_.state))
  val isCallEstablished = callStateOpt.map(_.contains(SelfConnected))
  val isCallOutgoing    = callStateOpt.map(_.contains(SelfCalling))
  val isCallIncoming    = callStateOpt.map(_.contains(OtherCalling))

  val isMuted           = currentCallOpt.map(_.exists(_.muted)).disableAutowiring()
  val isVideoCall       = currentCallOpt.map(_.exists(_.isVideoCall)).disableAutowiring()

  def startCall(account: UserId, conv: ConvId, withVideo: Boolean): Future[Unit] =
    if (PhoneUtils.getPhoneState(cxt) != PhoneState.IDLE) showErrorDialog(R.string.calling__cannot_start__title, R.string.calling__cannot_start__message)
    else {
      for {
        curCallZms  <- callingZms.head
        curCallConv <- callConvIdOpt.head
        //End any ongoing calls - if there is a current call, the convId must be defined
        _ <- curCallZms.fold(Future.successful({}))(_.calling.endCall(curCallConv.get))
        Some(newCallZms)  <- accounts.getZms(account)
        Some(newCallConv) <- newCallZms.convsStorage.get(conv)
        _ <- networkMode.head.map {
          case NetworkMode.OFFLINE           => showErrorDialog(R.string.alert_dialog__no_network__header, R.string.calling__call_drop__message)
          case NetworkMode._2G               => showErrorDialog(R.string.calling__slow_connection__title, R.string.calling__slow_connection__message)
          case NetworkMode.EDGE if withVideo => showErrorDialog(R.string.calling__slow_connection__title, R.string.calling__video_call__slow_connection__message).flatMap(_ => startCall(newCallConv, withVideo = true))
          case _                             => startCall(newCallConv, withVideo)
        }
      } yield {}
    }.recover {
      case NonFatal(e) =>
        error("Failed to start call", e)
    }

  private def startCall(c: ConversationData, withVideo: Boolean = false): Future[Unit] = {
    def call() = {
      currentCallOpt.head.map { incomingCall =>
        permissions.requestAllPermissions(if (incomingCall.map(_.isVideoCall).getOrElse(withVideo)) Set(CAMERA, RECORD_AUDIO) else Set(RECORD_AUDIO)).map {
          case true => callingService.head.map(_.startCall(c.id, withVideo))
          case false =>
            ViewUtils.showAlertDialog(
              cxt,
              R.string.calling__cannot_start__title,
              if (withVideo) R.string.calling__cannot_start__no_video_permission__message else R.string.calling__cannot_start__no_permission__message,
              R.string.permissions_denied_dialog_acknowledge,
              R.string.permissions_denied_dialog_settings,
              new DialogInterface.OnClickListener() {
                override def onClick(dialog: DialogInterface, which: Int): Unit =
                  if (incomingCall.isDefined) callingService.head.map(_.endCall(c.id))
              },
              new DialogInterface.OnClickListener() {
                override def onClick(dialog: DialogInterface, which: Int): Unit = {
                  returning(new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS, Uri.fromParts("package", cxt.getPackageName, null))) { i =>
                    i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
                    cxt.startActivity(i)
                  }
                  if (incomingCall.isDefined) callingService.head.map(_.endCall(c.id))
                }
              })
        } (Threading.Ui)
      }
    }

    if (c.convType == ConversationData.ConversationType.Group) conversationController.loadMembers(c.id).foreach { members =>
      if (members.size > 5)
        ViewUtils.showAlertDialog(
          cxt,
          getString(R.string.group_calling_title),
          getString(R.string.group_calling_message, Integer.valueOf(members.size)),
          getString(R.string.group_calling_confirm),
          getString(R.string.group_calling_cancel),
          new DialogInterface.OnClickListener() {
            def onClick(dialog: DialogInterface, which: Int) = call()
          }, null
        ) else call()
    }(Threading.Ui)
    else call()
  }

  for {
    cId <- convId
    incomingCall <- callState.map {
      case OtherCalling => true
      case _ => false
    }
    autoAnswer <- prefs.flatMap(_.preference(AutoAnswerCallPrefKey).signal)
  } if (incomingCall && autoAnswer) startCall(cId)





  /**
    * Opt Signals - these signals should be used where empty states (i.e, Nones) are important to the devices state. For example
    * if we have no zms instance, we should not have an active call. If we don't have a conversation, we shouldn't have an active
    * call. If there is no active call, for whatever reason, there should be no calling activity, and so on. The other signals
    * derived from these ones using `collect` will not propagate empty values, but that's okay since the calling UI should be
    * torn down before they can be of use to us anyway.
    *
    * Note, any signals which are PART of another signal (zipped or flatmapped) which needs to be aware of Nones should also
    * be careful to handle Nones themselves. This is because if a signal is collected, and the value is None, the individual
    * signal becomes empty, and so prevents the group of signals - of which it is a part of - from firing...
    */

  //The ZMessaging of the active call, or the currently active account if there is no active call, or none if no accounts are logged in.

  /**
    * ...And from here on is where only their proper value is important.
    */

  private var _wasUiActiveOnCallStart = false

  def wasUiActiveOnCallStart = _wasUiActiveOnCallStart

  val onCallStarted = isCallActive.onChanged.filter(_ == true).map { _ =>
    val active = ZMessaging.currentGlobal.lifecycle.uiActive.currentValue.getOrElse(false)
    _wasUiActiveOnCallStart = active
    active
  }

  onCallStarted.on(Threading.Ui) { _ =>
    CallingActivity.start(cxt)
  }(EventContext.Global)

  isCallEstablished.onChanged.filter(_ == true) { _ =>
    soundController.playCallEstablishedSound()
  }

  isCallActive.onChanged.filter(_ == false) { _ =>
    soundController.playCallEndedSound()
  }

  isCallActive.onChanged.filter(_ == false).on(Threading.Ui) { _ =>
    screenManager.releaseWakeLock()
  }(EventContext.Global)

  (for {
    v <- isVideoCall
    st <- callStateOpt
  } yield (v, st)) {
    case (true, _) => screenManager.setStayAwake()
    case (false, Some(OtherCalling)) => screenManager.setStayAwake()
    case (false, Some(SelfCalling | SelfJoining | SelfConnected)) => screenManager.setProximitySensorEnabled()
    case _ => screenManager.releaseWakeLock()
  }

  (for {
    m <- isMuted
    i <- isCallIncoming
  } yield (m, i)) { case (m, i) =>
    soundController.setIncomingRingTonePlaying(!m && i)
  }

  /**
    * From here on, put signals where we're not too worried if we handle empty states or not
    * (mostly stuff that wouldn't affect proper closing of a call).
    *
    * Most of the stuff here is just because it saves re-defining the signals in one of the sub-CallControllers
    */

  val zms = callingZms.collect { case Some(z) => z }

  val currentCall = currentCallOpt.collect { case Some(c) => c }

  val userStorage = zms map (_.usersStorage)
  val prefs = zms.map(_.prefs)

  val callingService = zms.map(_.calling).disableAutowiring()

  val convId = callConvIdOpt.collect { case Some(c) => c }

  val callingServiceAndCurrentConvId = (for {
    svc <- callingService
    c <- convId
  } yield (svc, c)).disableAutowiring()

  val callState = callStateOpt.collect { case Some(s) => s }

  val conversation = zms.zip(convId) flatMap { case (z, cId) => z.convsStorage.signal(cId) }
  val conversationName = conversation map (data => if (data.convType == IConversation.Type.GROUP) data.name.filter(!_.isEmpty).getOrElse(data.generatedName) else data.generatedName)

  val convDegraded = conversation.map(_.verified == Verification.UNVERIFIED)
    .orElse(Signal(false))
    .disableAutowiring()

  val degradationWarningText = convDegraded.flatMap {
    case false => Signal("")
    case true =>
      (for {
        zms <- zms
        convId <- convId
      } yield {
        zms.membersStorage.activeMembers(convId).flatMap { ids =>
          zms.usersStorage.listSignal(ids)
        }.map(_.filter(_.verified != Verification.VERIFIED).toList)
      }).flatten.map {
        case u1 :: u2 :: _ =>
          //TODO handle more than 2 users
          getString(R.string.conversation__degraded_confirmation__header__multiple_user, u1.name, u2.name)
        case List(u) =>
          //TODO handle string for case where user adds multiple clients
          getQuantityString(R.plurals.conversation__degraded_confirmation__header__single_user, 1, u.name)
        case _ => ""
      }
  }

  val degradationConfirmationText = convDegraded.flatMap {
    case false => Signal("")
    case true => isCallOutgoing.map {
      case true  => R.string.conversation__degraded_confirmation__place_call
      case false => R.string.conversation__degraded_confirmation__accept_call
    }.map(getString)
  }

  (for {
    v <- isVideoCall
    o <- isCallOutgoing
    d <- convDegraded
  } yield (v, o & !d)) { case (v, play) =>
    soundController.setOutgoingRingTonePlaying(play, v)
  }

  //Use Audio view to show conversation degraded screen for calling
  val showVideoView = convDegraded.flatMap {
    case true  => Signal(false)
    case false => isVideoCall
  }.disableAutowiring()

  val selfUser = zms flatMap (_.users.selfUser)

  val callerId = currentCallOpt flatMap {
    case Some(info) =>
      (info.others, info.state) match {
        case (_, Some(SelfCalling)) => selfUser.map(_.id)
        case (others, Some(OtherCalling)) if others.size == 1 => Signal.const(others.head)
        case _ => Signal.empty[UserId] //TODO Dean do I need this information for other call states?
      }
    case _ => Signal.empty[UserId]
  }

  val callerData = userStorage.zip(callerId).flatMap { case (storage, id) => storage.signal(id) }

  val groupCall = conversation.map(_.convType == ConversationType.Group)

}

object GlobalCallingController {

  def showOfflineDialog(implicit cxt: Context): Future[Unit] = {
    ViewUtils.showAlertDialog(
      cxt,
      R.string.alert_dialog__no_network__header,
      R.string.calling__call_drop__message,
      R.string.alert_dialog__confirmation,
      null, false)

    val p = Promise[Unit]()
    val dialog = new AlertDialog.Builder(context)
      .setCancelable(false)
      .setTitle(R.string.alert_dialog__no_network__header)
      .setMessage(msgRes)
      .setNeutralButton(android.R.string.ok, new DialogInterface.OnClickListener() {
        def onClick(dialog: DialogInterface, which: Int): Unit = {
          dialog.dismiss()
          p.complete(Success({}))
        }
      }).create
    dialog.show()
    p.future

  }

}

private class ScreenManager(implicit injector: Injector) extends Injectable {

  private val TAG = "CALLING_WAKE_LOCK"

  private val powerManager = Option(inject[PowerManager])

  private var stayAwake = false
  private var wakeLock: Option[PowerManager#WakeLock] = None

  def setStayAwake() = {
    (stayAwake, wakeLock) match {
      case (_, None) | (false, Some(_)) =>
        this.stayAwake = true
        createWakeLock();
      case _ => //already set
    }
  }

  def setProximitySensorEnabled() = {
    (stayAwake, wakeLock) match {
      case (_, None) | (true, Some(_)) =>
        this.stayAwake = false
        createWakeLock();
      case _ => //already set
    }
  }

  private def createWakeLock() = {
    val flags = if (stayAwake)
      DeprecationUtils.WAKE_LOCK_OPTIONS
    else PowerManager.PROXIMITY_SCREEN_OFF_WAKE_LOCK
    releaseWakeLock()
    wakeLock = powerManager.map(_.newWakeLock(flags, TAG))
    verbose(s"Creating wakelock")
    wakeLock.foreach(_.acquire())
    verbose(s"Aquiring wakelock")
  }

  def releaseWakeLock() = {
    for (wl <- wakeLock if wl.isHeld) {
      wl.release()
      verbose(s"Releasing wakelock")
    }
    wakeLock = None
  }
}

