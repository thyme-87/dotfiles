#!/bin/bash

#This script is to be used, to install the "basic packages"
#that are needed to be able to continue with an ansible based
#installation

#TODO check if this script is running on arch linux and abort if not

BASE_PACKAGES_NETWORK="openssh openssl bind iwd net-tools dhcpcd rsync"
BASE_PACKAGES="base-devel git ansible vim python-pip man"

echo "Installing packages required for network, wifi etc."
pacman -S ${BASE_PACKAGES_NETWORK}

echo "Installing basic packages required for continuing with an ansible based system setup"
pacman -S ${BASE_PACKAGES}
