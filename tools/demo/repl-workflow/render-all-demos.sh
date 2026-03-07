#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v vhs >/dev/null 2>&1; then
  echo "Missing required command: vhs" >&2
  exit 1
fi

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "Missing required command: ffmpeg" >&2
  exit 1
fi

demo_width="${DEMO_WIDTH:-960}"
demo_fps="${DEMO_FPS:-12}"
demo_crf="${DEMO_CRF:-30}"
demo_preset="${DEMO_PRESET:-slow}"
gif_fps="${DEMO_GIF_FPS:-12}"

if [ "$#" -gt 0 ]; then
  tapes=()
  for name in "$@"; do
    case "$name" in
      *.tape) tapes+=("$script_dir/$name") ;;
      *) tapes+=("$script_dir/$name.tape") ;;
    esac
  done
else
  mapfile -t tapes < <(find "$script_dir" -maxdepth 1 -type f -name '*.tape' | sort)
fi

if [ "${#tapes[@]}" -eq 0 ]; then
  echo "No tape files found." >&2
  exit 1
fi

for tape in "${tapes[@]}"; do
  if [ ! -f "$tape" ]; then
    echo "Tape not found: $tape" >&2
    exit 1
  fi

  mp4="${tape%.tape}.mp4"
  gif="${tape%.tape}.gif"
  tmp_mp4="${mp4%.mp4}.compressed.mp4"

  echo "Rendering $(basename "$tape")"
  vhs "$tape"

  echo "Compressing $(basename "$mp4")"
  ffmpeg -y \
    -i "$mp4" \
    -vf "fps=$demo_fps,scale=$demo_width:-2:flags=lanczos,format=yuv420p" \
    -an \
    -c:v libx264 \
    -preset "$demo_preset" \
    -crf "$demo_crf" \
    -movflags +faststart \
    "$tmp_mp4"
  mv "$tmp_mp4" "$mp4"

  echo "Creating $(basename "$gif") from $(basename "$mp4")"
  ffmpeg -y \
    -i "$mp4" \
    -vf "fps=$gif_fps,scale=$demo_width:-1:flags=lanczos" \
    "$gif"
done

echo "Rendered ${#tapes[@]} demo(s) at ${demo_width}px / ${demo_fps}fps / CRF ${demo_crf}."
