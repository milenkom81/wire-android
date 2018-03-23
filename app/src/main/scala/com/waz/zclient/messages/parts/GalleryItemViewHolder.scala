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
package com.waz.zclient.messages.parts

import java.io.File

import android.content.Context
import android.support.v7.widget.RecyclerView
import android.util.AttributeSet
import android.widget.ImageView
import com.waz.utils.wrappers.{AndroidURIUtil, URI}
import com.waz.zclient.ViewHelper
import com.waz.zclient.common.views.ImageAssetDrawable
import com.waz.zclient.pages.extendedcursor.image.CursorImagesLayout
import com.waz.zclient.utils.RichView

class GalleryItemViewHolder(imageView: CursorGalleryItem) extends RecyclerView.ViewHolder(imageView) {

  def bind(path: String, callback: CursorImagesLayout.Callback): Unit = {
    val uri = AndroidURIUtil.fromFile(new File(path))
    imageView.setImage(uri)
    imageView.onClick(callback.onGalleryPictureSelected(uri))
  }
}

class CursorGalleryItem(context: Context, attrs: AttributeSet, defStyleAttr: Int) extends ImageView(context, attrs, defStyleAttr) with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  def setImage(uri: URI): Unit = setImageDrawable(ImageAssetDrawable(uri))

  override protected def onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int): Unit =
    super.onMeasure(heightMeasureSpec, heightMeasureSpec) // to make it square
}
