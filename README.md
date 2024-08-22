
# Chemical Process Fault Detection Using Long Short-Term Memory Recurrent Neural Network

**[22/08/2024 UPDATE] Thanks to [@SamuelBFG](https://github.com/SamuelBFG) that fixed the ported version for TF 2.0 and also contributed with a ported version for PyTorch.

**[24/4/2022 UPDATE] The usage of the [`tep`](https://github.com/gmxavier/TEP-meets-LSTM/tree/master/tep), a TEP wrapper for R, is now available in this [notebook](https://github.com/gmxavier/TEP-meets-LSTM/blob/master/using-tep.ipynb).**

**[27/9/2021 UPDATE] Thanks to [@antonionicampos](https://github.com/antonionicampos) that contributed with a ported version for TF 2.0. By the way, if you are interested in a TEP wrapper for Python, I recommend the [`tep2py`](https://github.com/camaramm/tep2py) developed by [@camaramm](https://github.com/camaramm) and based on the same modified FORTRAN code used by [`tep`](https://github.com/gmxavier/TEP-meets-LSTM/tree/master/tep).**

**[19/8/2021 UPDATE] I've added some chunk of code that allows running this [notebook](https://github.com/gmxavier/TEP-meets-LSTM/blob/master/tep-meets-lstm.ipynb) in the Colab environment.**

This repository contains everything needed to reproduce the results presented in the following paper:

> Gilberto M. Xavier and José Manoel de Seixas. Fault Detection and Diagnosis in a Chemical Process using Long Short-Term Memory Recurrent Neural Network. 31st International Joint Conference on Neural Networks, IJCNN 2018. Rio de Janeiro, Brazil 8-13 July 2018.

Feel free to contribute with this initiative and if you develop derived works based on these codes, please cite the already mentioned paper.

The figure below shows how the new approach (LSTM) presented in this paper outperforms several already published fault detection and diagnosis methods regarding the Fault 3, which is a notable difficult one.

<p align="center">
  <img width="800" height="600" src="https://github.com/gmxavier/TEP-meets-LSTM/blob/master/performance_comparison.png">
</p>

# Bonus

If you need a CC licensed picture of the TE-process flowsheet for your derived work, please feel free to use the following one.

![alt text](https://github.com/gmxavier/TEP-meets-LSTM/blob/master/tep_flowsheet.png)
