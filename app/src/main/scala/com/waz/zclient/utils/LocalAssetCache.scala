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
package com.waz.zclient.utils

import com.waz.model.AssetData
import com.waz.utils.TrimmingLruCache.CacheSize
import com.waz.utils.wrappers.{Context, URI}
import com.waz.utils.{Cache, TrimmingLruCache, returning}

class LocalAssetCache(lru: Cache[URI, AssetData]) {
  def getOrCreateAsset(uri: URI): AssetData = Option(lru.get(uri)).getOrElse(
    returning(AssetData.newImageAssetFromUri(uri = uri))(lru.put(uri, _))
  )
}

object LocalAssetCache {
  def apply(context: Context) = new LocalAssetCache(
    new TrimmingLruCache[URI, AssetData](context, CacheSize(total => math.max(5 * 1024 * 1024, (total - 30 * 1024 * 1024) / 2))) {
      override def sizeOf(id: URI, value: AssetData): Int = value.size.toInt
    }
  )
}
