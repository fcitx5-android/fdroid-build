#!/bin/bash

cat << EOF > build.cfg
project_name = fcitx5-android-plugin-pinyin-zhwiki
package_name = org.fcitx.fcitx5.android.plugin.pinyin_zhwiki
aboutlibraries_version = 10.7.0
main_version = 30f4f48192
desugarJDKLibs_version = 2.0.3
kotlin_version = 1.8.20
android_version = 8.0.1
build_version_name = $ZHWIKI_DICT_VER
version_code = $ZHWIKI_DICT_VER
build_commit_hash = unknown
app_name_debug = Fcitx5 for Android (zhwiki dict | Debug)
app_name_release = Fcitx5 for Android (zhwiki dict)
plugin_description = Fcitx 5 Pinyin Dictionary from zh.wikipedia.org
plugin_api_version = 0.1
EOF

stack exec --package shake --package mustache -- runghc Main.hs
mkdir -p out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/
curl -L "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/${ZHWIKI_CONVERTER_VER}/zhwiki-${ZHWIKI_DICT_VER}.dict" \
  -o "out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/zhwiki-${ZHWIKI_DICT_VER}.dict"
cd out
./gradlew assembleRelease
mv app/build/outputs/apk/release/*.apk "org.fcitx.fcitx5.android.plugin.pinyin_zhwiki-${ZHWIKI_DICT_VER}-unsigned.apk"