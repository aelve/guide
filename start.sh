#!/bin/bash

tmux new-session -d -s guide
tmux send-keys 'stack exec guide' 'C-m'
tmux split-window -h
tmux send-keys 'cd front && npm start' 'C-m'

tmux attach-session -t guide
