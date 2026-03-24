#!/usr/bin/env bash
# gstack-customize.sh — Rename all gstack skills to /g-{name} to avoid
# collisions with built-in Claude Code slash commands.
#
# Run after gstack install or upgrade. Lives OUTSIDE the gstack git repo
# so it survives `git reset --hard` during upgrades.
#
# Usage: ~/.claude/skills/gstack-customize.sh [--verify]
set -euo pipefail

GSTACK_DIR="${HOME}/.claude/skills/gstack"
VERIFY_ONLY=0

if [[ "${1:-}" == "--verify" ]]; then
  VERIFY_ONLY=1
fi

if [[ ! -d "$GSTACK_DIR" ]]; then
  echo "ERROR: gstack not found at $GSTACK_DIR" >&2
  exit 1
fi

# All skill names to prefix (order: longer names first to avoid partial matches)
SKILLS=(
  "setup-browser-cookies"
  "design-consultation"
  "plan-design-review"
  "document-release"
  "land-and-deploy"
  "plan-ceo-review"
  "plan-eng-review"
  "gstack-upgrade"
  "design-review"
  "office-hours"
  "setup-deploy"
  "investigate"
  "benchmark"
  "autoplan"
  "careful"
  "canary"
  "browse"
  "codex"
  "freeze"
  "guard"
  "qa-only"
  "review"
  "retro"
  "ship"
  "cso"
  "qa"
  "unfreeze"
)

# ─── Verify mode ──────────────────────────────────────────────
if [[ "$VERIFY_ONLY" -eq 1 ]]; then
  echo "Verifying gstack skill names..."
  ERRORS=0
  for f in $(find "$GSTACK_DIR" -name "SKILL.md" -maxdepth 2 | sort); do
    name=$(grep '^name:' "$f" | head -1 | sed 's/name: *//')
    # Root SKILL.md should be "gstack", all others should be "g-*"
    if [[ "$f" == "$GSTACK_DIR/SKILL.md" ]]; then
      if [[ "$name" != "gstack" ]]; then
        echo "  FAIL: $f has name '$name' (expected 'gstack')"
        ERRORS=$((ERRORS + 1))
      else
        echo "  OK: $f -> $name"
      fi
    else
      if [[ "$name" != g-* ]]; then
        echo "  FAIL: $f has name '$name' (expected 'g-$name')"
        ERRORS=$((ERRORS + 1))
      else
        echo "  OK: $f -> $name"
      fi
    fi
  done

  if [[ "$ERRORS" -gt 0 ]]; then
    echo ""
    echo "FAILED: $ERRORS skill(s) not renamed. Run without --verify to fix."
    exit 1
  else
    echo ""
    echo "ALL GOOD: All skills have g-* prefix."
    exit 0
  fi
fi

# ─── Rename mode ──────────────────────────────────────────────
echo "Renaming gstack skills to g-* prefix..."

# Step 1: Rename frontmatter `name:` field
RENAMED=0
for f in $(find "$GSTACK_DIR" -name "SKILL.md" -maxdepth 2); do
  old_name=$(grep '^name:' "$f" | head -1 | sed 's/name: *//')

  # Skip root SKILL.md (stays as "gstack")
  [[ "$old_name" == "gstack" ]] && continue

  # Skip if already prefixed
  [[ "$old_name" == g-* ]] && continue

  new_name="g-$old_name"
  sed -i '' "s/^name: ${old_name}$/name: ${new_name}/" "$f"
  RENAMED=$((RENAMED + 1))
done
echo "  Renamed $RENAMED skill frontmatter entries"

# Step 2: Update cross-references (/skill-name -> /g-skill-name)
for skill in "${SKILLS[@]}"; do
  find "$GSTACK_DIR" -name "SKILL.md" -maxdepth 2 \
    -not -path "*/.agents/*" \
    -exec perl -pi -e "
      s{(?<![/a-zA-Z])/(${skill})(?=[^a-zA-Z0-9-]|\$)}{/g-\$1}g;
    " {} \;
done
echo "  Updated cross-references"

# Step 3: Suppress onboarding prompts and telemetry
mkdir -p ~/.gstack
touch ~/.gstack/.completeness-intro-seen
touch ~/.gstack/.telemetry-prompted
~/.claude/skills/gstack/bin/gstack-config set telemetry off 2>/dev/null || true
~/.claude/skills/gstack/bin/gstack-config set proactive false 2>/dev/null || true
echo "  Suppressed onboarding + telemetry"

# Step 4: Verify
echo ""
exec "$0" --verify
