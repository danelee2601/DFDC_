"""
Run `dfdc.exe`.

Findings:
- You can type a command line by "command1 \n command2 \n". The key is `\n` which is equivalent to the enter key.

References:
[1] Stackoverflow, https://stackoverflow.com/questions/163542/how-do-i-pass-a-string-into-subprocess-popen-using-the-stdin-argument
"""
import os
import re
import subprocess
from subprocess import PIPE
import json

import numpy as np
import matplotlib.pyplot as plt
from bayes_opt import BayesianOptimization, UtilityFunction

from python_scripts.config import *

origin_cwd = os.getcwd()
root = os.path.dirname(origin_cwd)
os.chdir(os.path.join(root, 'bin'))


def _define_optimizer_n_utiliy():
    optimizer = BayesianOptimization(
        f=None,
        pbounds=pbounds,
        verbose=2,
        random_state=1
    )
    #utility = UtilityFunction('ei', xi=0.01, kappa=2.576)\
    utility = UtilityFunction(kind="ucb", kappa=10, xi=0.0)
    return optimizer, utility


def _run_dfdc(commands):
    """
    Run `dfdc.exe` and return `thrust` and `torque`
    """
    commands = "\n".join(commands) + "\n"
    p = subprocess.run('dfdc.exe', stdout=PIPE, stderr=PIPE,
                       input=commands, encoding='latin-1')
    out = p.stdout
    thrust = float(re.findall(r'Thrust\(N\)\s+\:\s+(\d+.\d*?)', out)[0])
    torque = float(re.findall(r'torQue\(N-m\)\:\s+(\d+.\d*)', out)[0])

    return thrust, torque


def _cost_func(thrust, torque):
    J = thrust
    return J


def _get_new_case_fname():
    fname, ext = case_fname.split('.')
    new_case_fname = fname + '_.' + ext
    return new_case_fname


def _update_case_file(case_fname, next_point_to_probe):
    """
    1. open the existing case file.
    2. open a new case file with a slightly different file name.
    3. copy the existing case file's content on the new case file except for the `r, Chord, Beta` part.
    """
    dist = [0.8, 0.11, 0.14, 0.17, 0.2, 0.23, 0.26, 0.29, 0.32, 0.35]  # fixed
    new_case_fname = _get_new_case_fname()

    if os.path.isfile(new_case_fname):
        os.unlink(new_case_fname)

    f_new = open(new_case_fname, 'a')
    with open(case_fname, 'r') as f_exist:
        count = 1
        modify = False
        for line in f_exist:
            if 'Chord' in line:
                f_new.write(line)
                modify = True
            if not modify:
                if count == 11:
                    count += 1
                    continue
                else:
                    f_new.write(line)
            else:
                f_new.write(
                    f"{dist[count - 1]} {next_point_to_probe[f'chord{count}']} {next_point_to_probe[f'beta{count}']}\n")
                count += 1
                if count > 10:
                    modify = False
    f_new.close()


def _update_BO(optimizer, next_point_to_probe, J):
    """
    update the Baysoan optimizer in a direction where `J` is maximized.
    """
    optimizer.register(
        params=next_point_to_probe,
        target=J,
    )


def _random_suggest():
    next_point_to_probe = {}
    for k, v in pbounds.items():
        next_point_to_probe[k] = np.random.uniform(v[0], v[1])
    return next_point_to_probe


def _print_status(J, iter):
    if iter % print_interval == 0:
        print(f'{iter} | J: {round(J, 2)}')


def _result_plot(optim_results):
    plt.figure(figsize=(10, 4))
    plt.plot(optim_results['iters'], optim_results['Js'])
    plt.xlabel('iters'); plt.ylabel('J (cost)')
    plt.savefig(os.path.join(origin_cwd, 'J_history.png'))
    plt.show()


def _export_optimal_input_json(optimal_input):
    with open(os.path.join(origin_cwd, "optimal_input.json"), "w") as json_file:
        json.dump(optimal_input, json_file)


def main():
    optimizer, utility = _define_optimizer_n_utiliy()

    optim_results = {'iters': [], 'Js': []}
    next_point_to_probe = optimizer.suggest(utility)
    for iter in range(1, n_iters + 1):
        _update_case_file(case_fname, next_point_to_probe)
        new_case_fname = _get_new_case_fname()
        commands = get_commands(new_case_fname)
        thrust, torque = _run_dfdc(commands)
        J = _cost_func(thrust, torque)
        _update_BO(optimizer, next_point_to_probe, J)
        next_point_to_probe = optimizer.suggest(utility) if iter > init_random_steps else _random_suggest()
        _print_status(J, iter)

        optim_results['iters'].append(iter)
        optim_results['Js'].append(J)

    _result_plot(optim_results)
    optimal_input = optimizer.max
    print("\n\n# Optimal:\n", optimal_input)
    _export_optimal_input_json(optimal_input)


if __name__ == "__main__":
    main()