#!/usr/bin/env bash
set -euo pipefail

./nemoage ini_files/run/dispersed_p10_r01_k30_b0.03.ini

./nemoage ini_files/run/one_cluster_p10_r01_k30_b0.03.ini

./nemoage ini_files/run/multi_cluster_p10_r01_k30_b0.03.ini

./nemoage ini_files/run/transects_p10_r01_k30_b0.03.ini

./nemoage ini_files/run/dispersed_p25_r01_k30_b0.03.ini

./nemoage ini_files/run/one_cluster_p25_r01_k30_b0.03.ini

./nemoage ini_files/run/multi_cluster_p25_r01_k30_b0.03.ini

./nemoage ini_files/run/transects_p25_r01_k30_b0.03.ini

./nemoage ini_files/run/dispersed_p40_r01_k30_b0.03.ini

./nemoage ini_files/run/one_cluster_p40_r01_k30_b0.03.ini

./nemoage ini_files/run/multi_cluster_p40_r01_k30_b0.03.ini

./nemoage ini_files/run/transects_p40_r01_k30_b0.03.ini

