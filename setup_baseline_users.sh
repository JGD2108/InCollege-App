#!/bin/bash

# Create baseline USERS.DAT with all required test users
# Format: username (padded to 12 chars) followed by password

cat > /workspace/USERS.DAT.baseline << 'EOF'
LoginUser1  Login@11
JobUser     JobUser@1
SkillsUser  Skills@11
WelcomeUser Welcome@1
JourneyUser Journey@1
EOF

echo "Baseline USERS.DAT created with test users"
cat /workspace/USERS.DAT.baseline
