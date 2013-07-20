#!/bin/bash
#
# Copyright 2011 Google Inc. All Rights Reserved.
# Author: bobgardner@google.com (Bob Gardner)
#
# Toggles the "focus follows mouse" setting.

focus_mode=$(gconftool-2 --get /apps/metacity/general/focus_mode)

if [ "$focus_mode" = "click" ]; then
  gconftool-2 --set /apps/metacity/general/focus_mode -t string sloppy
else
  gconftool-2 --set /apps/metacity/general/focus_mode -t string click
fi
