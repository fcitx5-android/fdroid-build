#!/bin/env bash

cat << EOF > build.cfg
project_name = fcitx5-android-plugin-pinyin-zhwiki
package_name = org.fcitx.fcitx5.android.plugin.pinyin_zhwiki
aboutlibraries_version = 10.8.3
main_version = 0.0.7
desugarJDKLibs_version = 2.0.3
kotlin_version = 1.9.0
android_version = 8.1.1
build_version_name = ${ZHWIKI_DICT_VER:-test}
version_code = ${ZHWIKI_DICT_VER:-0}
build_commit_hash = unknown
app_name_debug = Fcitx5 for Android (zhwiki dict | Debug)
app_name_release = Fcitx5 for Android (zhwiki dict)
plugin_description = Fcitx 5 Pinyin Dictionary from zh.wikipedia.org
plugin_api_version = 0.1
EOF

nix run
# nix flake needs git add
git add -f out
mkdir -p out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/
curl -L "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/${ZHWIKI_CONVERTER_VER}/zhwiki-${ZHWIKI_DICT_VER}.dict" \
  -o "out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/zhwiki-${ZHWIKI_DICT_VER}.dict"
cd out
nix develop .#noAS --command ./gradlew assembleRelease
mv app/build/outputs/apk/release/*.apk "org.fcitx.fcitx5.android.plugin.pinyin_zhwiki-${ZHWIKI_DICT_VER}-unsigned.apk"
