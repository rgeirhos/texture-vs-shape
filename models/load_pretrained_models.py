"""
Read PyTorch model from .pth.tar checkpoint.
"""
import os
import sys
from collections import OrderedDict
import torch
import torchvision
import torchvision.models
from torch.utils import model_zoo



def load_model(model_name):

    model_urls = {
            'resnet50': 'https://bitbucket.org/robert_geirhos/texture-vs-shape-pretrained-models/raw/6f41d2e86fc60566f78de64ecff35cc61eb6436f/resnet50_train_60_epochs-c8e5653e.pth.tar',
    }

    model = torchvision.models.resnet50(pretrained=False)
    model = torch.nn.DataParallel(model).cuda()
    checkpoint = model_zoo.load_url(model_urls[model_name])
    model.load_state_dict(checkpoint["state_dict"])
    return model


if __name__ == "__main__":

    model_name = "resnet50" # ResNet-50 trained for 60 epochs on Stylized-ImageNet (SIN).
    model = load_model(model_name)

 
    for k, v in model.module.state_dict().items():
        print(k)

    print("Model loading completed.")
