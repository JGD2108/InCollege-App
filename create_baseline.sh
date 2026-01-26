#!/bin/bash

# Create baseline USERS.DAT by running the program to create each user
cd /workspace

# Clear USERS.DAT
> USERS.DAT

# Array of username password pairs
users=(
    "LoginUser1:Login@11"
    "JobUser:JobUser@1"
    "SkillsUser:Skills@11"
    "WelcomeUser:Welcome@1"
    "MenuUser:Menu@123"
    "PersistMenu:Persist@1"
    "FindUser:FindUsr@1"
    "SkillUser:SkillU@11"
    "ReturnUser:Return@11"
    "FiveSkills:FiveSk@11"
    "GoBackUser:GoBack@11"
    "NavUser:NavUse@11"
    "InvalidUser:Invalid@1"
    "MultiUser:Good@123"
    "ValidUser:AnyPwd@11"
    "RetryUser:Correct@1"
)

# Create each user
for user_pass in "${users[@]}"; do
    username="${user_pass%%:*}"
    password="${user_pass##*:}"
    
    echo "Creating user: $username"
    echo -e "2\n$username\n$password\n3" > INPUT.DAT
    /workspace/bin/InCollege > /dev/null 2>&1
done

echo "Baseline USERS.DAT created:"
cat USERS.DAT

# Copy to baseline
cp USERS.DAT USERS.DAT.baseline
echo "Copied to USERS.DAT.baseline"
