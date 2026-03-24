---
name: g-upgrade
version: 1.0.0
description: |
  MANUAL TRIGGER ONLY: invoke only when user types /g-upgrade.
  Upgrade gstack to latest version and re-apply g-* prefix customizations.
  Use instead of /gstack-upgrade to preserve skill renames.
allowed-tools:
  - Bash
  - Read
---

# /g-upgrade — Upgrade gstack + re-apply customizations

Upgrades gstack from git, rebuilds, and re-applies the `g-*` skill name
prefixes so they don't collide with built-in Claude Code commands.

## Step 1: Detect current version

```bash
GSTACK_DIR="$HOME/.claude/skills/gstack"
OLD_VERSION=$(cat "$GSTACK_DIR/VERSION" 2>/dev/null || echo "unknown")
echo "Current version: $OLD_VERSION"
```

## Step 2: Pull latest and rebuild

```bash
cd "$HOME/.claude/skills/gstack"
git fetch origin
git reset --hard origin/main
./setup
```

Show the user any build output or errors. If `./setup` fails, stop and report.

## Step 3: Re-apply g-* customizations

```bash
"$HOME/.claude/skills/gstack-customize.sh"
```

This renames all skill frontmatter names to `g-{name}`, updates
cross-references, and verifies the result. If verification fails, report
the specific failures.

## Step 4: Show what changed

```bash
NEW_VERSION=$(cat "$HOME/.claude/skills/gstack/VERSION" 2>/dev/null || echo "unknown")
echo "Upgraded: $OLD_VERSION -> $NEW_VERSION"
```

If the version changed, read `$HOME/.claude/skills/gstack/CHANGELOG.md` and
summarize the new entries (5-7 bullets). If the version didn't change, say
"Already on latest version, re-applied customizations."
