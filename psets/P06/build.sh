#!/bin/bash

PROJECT_NAME="419004279_P06"

# Create temp project folder
mkdir $PROJECT_NAME

# Move files to temp folder
cp *.hs $PROJECT_NAME/
cp README.md $PROJECT_NAME/
cp -r data $PROJECT_NAME/

# Print temp file content
tree $PROJECT_NAME/

# Create compressed files
tar czf $PROJECT_NAME.tar.gz $PROJECT_NAME

rm -rf $PROJECT_NAME/
