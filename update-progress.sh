#!/bin/bash

# Count HTML files in games directory
GAME_COUNT=$(find games -name "*.html" -type f | wc -l | tr -d ' ')

echo "Found $GAME_COUNT games"

# Update README.md
sed -i.bak "s/\*\*Current Progress:\*\* [0-9]\+\/100 games completed/**Current Progress:** $GAME_COUNT\/100 games completed/" README.md

# Remove backup file
rm -f README.md.bak

echo "Updated README.md with progress: $GAME_COUNT/100"
