#!/bin/bash

cat << EOF > build.cfg
project_name = fcitx5-android-plugin-pinyin-moegirl
package_name = org.fcitx.fcitx5.android.plugin.pinyin_moegirl
aboutlibraries_version = 10.8.0
main_version = 0.0.6-29-g00034d9
desugarJDKLibs_version = 2.0.3
kotlin_version = 1.8.22
android_version = 8.0.2
build_version_name = $MOEGIRL_VER
version_code = $MOEGIRL_VER
build_commit_hash = unknown
app_name_debug = Fcitx5 for Android (moegirl dict | Debug)
app_name_release = Fcitx5 for Android (moegirl dict)
plugin_description = Fcitx 5 Pinyin Dictionary from zh.moegirl.org.cn
plugin_api_version = 0.1
EOF

stack exec --package shake --package mustache -- runghc Main.hs
mkdir -p out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/
curl -L "https://github.com/outloudvi/mw2fcitx/releases/download/${MOEGIRL_VER}/moegirl.dict" \
  -o "out/app/src/main/assets/usr/share/fcitx5/pinyin/dictionaries/moegirl-${MOEGIRL_VER}.dict"
cd out
./gradlew assembleRelease
mv app/build/outputs/apk/release/*.apk "org.fcitx.fcitx5.android.plugin.pinyin_moegirl-${MOEGIRL_VER}-unsigned.apk"
