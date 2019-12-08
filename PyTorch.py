import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import Dataset, DataLoader
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
import pdb
torch.set_printoptions(linewidth=120)

def get_num_correct(preds, labels):
    return preds.argmax(dim=1).eq(labels).sum().item()

class Network(nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = nn.Conv2d(in_channels=1, out_channels=6, kernel_size=5)
        self.conv2 = nn.Conv2d(in_channels=6, out_channels=12, kernel_size=5)
        self.fc1 = nn.Linear(in_features=12 * 4 * 4, out_features=120)
        self.fc2 = nn.Linear(in_features=120, out_features=10)
        self.out = nn.Linear(in_features=60, out_features=10)

    def forward(self, t):
        # (1) input layer
        t = t

        # (2) hidden conv layer
        t = self.conv1(t)
        t = F.relu(t)
        t = F.max_pool2d(t, kernal_size=2, stride=2)



#https://deeplizard.com/learn/video/0VCOG8IeVf8

train_set = torchvision.datasets.FashionMNIST(
    root='./data'
    ,train=True
    ,download=True
    ,transform=transforms.Compose([
        transforms.ToTensor()
    ])
)




#################################################################
## csv dataloader
## https://averdones.github.io/reading-tabular-data-with-pytorch-and-training-a-multilayer-perceptron/
## https://towardsdatascience.com/learn-enough-python-to-be-useful-argparse-e482e1764e05
#################################################################

class CsvLoader(Dataset):
    """Csv files"""

    def __init__(self, csv_file, dep_var):
        """Initializes instance of class CsvLoader.

        Args:
            csv_file (str): Path to the csv file required.
            dep_var (str): Dependent variable
        """
        self.df = pd.read_csv(csv_file)
        self.dep_var = dep_var

        # Save dependent variable and predictors
        self.x = self.df.drop(self.dep_var, axis=1)
        self.y = self.df[self.dep_var]

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        # Convert idx from tensor to list due to pandas bug (that arises when using pytorch's random_split)
        if isinstance(idx, torch.Tensor):
            idx = idx.tolist()

        return [self.x.iloc[idx].values, self.y[idx]]


# Pytorch csv loader function
csv_file = "https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/master/econ_fin_data.csv"
data = CsvLoader(csv_file, 'y1')

# Review data
for i in range(len(data)):
    sample = data[i]
    print(sample)
    if i == 3:
        break


# Load csv locally
csv_file1 = "C:/Users/brent/Documents/R/Misc_scripts/Shiller.csv"
df1 = pd.read_csv(csv_file1)

# Load csv from github
csv_file2 = "https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/master/econ_fin_data.csv"
df2 = pd.read_csv(csv_file2)

