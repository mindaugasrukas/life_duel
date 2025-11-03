#!/bin/bash
set -e

# backup old model
if [ -f "../../games/tfjs_model/model.json" ]; then
    timestamp=$(date +"%Y%m%d_%H%M%S")
    mv ../../games/tfjs_model ../../games/backup/tfjs_model_backup_$timestamp
    echo "Backed up old model to ../../games/backup/tfjs_model_backup_$timestamp"
fi

# # Export PyTorch model to ONNX
# MODEL=${1:-dqn_tictactoe_model.pth}
# python3 <<EOF
# import torch
# from dqn_tictactoe_selfplay import DQNAgent
# model = torch.load("$MODEL", map_location="cpu", weights_only=False)
# state_dict = model["q_state_dict"] if "q_state_dict" in model else model
# agent = DQNAgent()
# agent.q.load_state_dict(state_dict)
# dummy_input = torch.zeros(1, 9)
# torch.onnx.export(agent.q, dummy_input, "dqn_tictactoe.onnx", input_names=['input'], output_names=['output'], opset_version=17)
# print("Exported dqn_tictactoe.onnx")
# EOF

# # Install required packages
# pyenv install 3.11  # if not already installed
# pyenv virtualenv 3.11 tfconvert-3.11
# pyenv activate tfconvert-3.11
# pip install torch "onnx>=1.14.0,<1.17.0" onnx-tf tensorflow==2.15 tensorflow-addons tensorflowjs "tensorflow-probability<0.24"

# Convert ONNX model to TensorFlow SavedModel format
onnx-tf convert -i dqn_tictactoe.onnx -o tf_saved_model

# Convert TensorFlow SavedModel to TensorFlow.js format
tensorflowjs_converter --input_format=tf_saved_model --output_format=tfjs_graph_model tf_saved_model ../../games/tfjs_model

echo "Conversion complete. TensorFlow.js model is in ../../games/tfjs_model/"
