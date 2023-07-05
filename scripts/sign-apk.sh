#!/bin/bash

BUILD_TOOLS_ROOT="${ANDROID_HOME}/build-tools/${BUILD_TOOLS_VERSION:-33.0.2}"

ZIPALIGN_BIN="${BUILD_TOOLS_ROOT}/zipalign"
APKSIGNER_BIN="${BUILD_TOOLS_ROOT}/apksigner"

unsigned="$1"
aligned="${unsigned/.apk/-aligned.apk}"
signed="${unsigned/-unsigned.apk/.apk}"

KEY_FILE=$(mktemp)
base64 -d "${SIGN_KEY_BASE64}" > "${KEY_FILE}"

"$ZIPALIGN_BIN" -p 4 "$unsigned" "$aligned"
"$APKSIGNER_BIN" sign --out "$signed" \
    --ks "$KEY_FILE" \
    --ks-key-alias "$SIGN_KEY_ALIAS" \
    --ks-pass "env:SIGN_KEY_PWD" \
    "$aligned"

rm "${KEY_FILE}"
echo "$signed"
