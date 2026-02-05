#!/bin/bash

# Script to update test INPUT and OUTPUT files to use standard test users

# Define the 5 standard users
declare -A USERS
USERS[NM-8]="TestUser1 Test@123"
USERS[NM-12]="TestUser2 Test@456"
USERS[NM-13]="TestUser3 Test@789"
USERS[NM-14]="TestUser4 Test@abc"
USERS[NM-15]="TestUser5 Test@def"

# Function to update a test's InCollege-Input.txt file
update_input_file() {
    local input_file="$1"
    local new_username="$2"
    local new_password="$3"
    local feature="$4"

    # Check if file exists and starts with "1" (login option)
    if [ ! -f "$input_file" ]; then
        return
    fi

    local first_line=$(head -n 1 "$input_file")
    if [ "$first_line" != "1" ]; then
        return
    fi

    echo "Updating: $input_file"

    # Read the file into an array
    mapfile -t lines < "$input_file"

    # Update line 2 (username) and line 3 (password) if they exist
    if [ ${#lines[@]} -ge 2 ]; then
        lines[1]="$new_username"
    fi
    if [ ${#lines[@]} -ge 3 ]; then
        lines[2]="$new_password"
    fi

    # Write back to file
    printf "%s\n" "${lines[@]}" > "$input_file"
}

# Function to update InCollege-Output.txt file
update_output_file() {
    local output_file="$1"
    local old_username="$2"
    local new_username="$3"
    local old_password="$4"
    local new_password="$5"

    if [ ! -f "$output_file" ]; then
        return
    fi

    echo "Updating: $output_file"

    # Replace old username/password with new ones
    sed -i "s/$old_username/$new_username/g" "$output_file"
    if [ "$old_password" != "$new_password" ]; then
        sed -i "s/$old_password/$new_password/g" "$output_file"
    fi
}

echo "Starting test file updates..."
echo "================================"

# Process each feature
for feature in NM-8 NM-12 NM-13 NM-14 NM-15; do
    read new_username new_password <<< "${USERS[$feature]}"
    echo ""
    echo "Processing $feature tests..."
    echo "New credentials: $new_username / $new_password"

    # Find all test directories for this feature
    for test_dir in /workspace/Tests/Epic1/${feature}_*/TC-*/; do
        if [ -d "$test_dir" ]; then
            input_file="${test_dir}InCollege-Input.txt"
            output_file="${test_dir}InCollege-Output.txt"

            # Get old credentials from INPUT.DAT
            if [ -f "$input_file" ]; then
                old_username=$(sed -n '2p' "$input_file" 2>/dev/null)
                old_password=$(sed -n '3p' "$input_file" 2>/dev/null)

                # Update files
                update_input_file "$input_file" "$new_username" "$new_password" "$feature"
                update_output_file "$output_file" "$old_username" "$new_username" "$old_password" "$new_password"
            fi
        fi
    done
done

echo ""
echo "================================"
echo "Test files updated successfully!"

