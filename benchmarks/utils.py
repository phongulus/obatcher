import subprocess
import matplotlib.pyplot as plt
from tqdm.notebook import tqdm

def run_process(name, no_iters=5, count=1_000_000,
                domains=16,
                validate=False,
                verbose=False,
                init_count=None,
                sorted=None,
                no_searches=None,
                search_threshold=None,
                insert_threshold=None,
                branching_factor=None
                ):
    cmd = ["../_build/default/benchmarks/bench.exe", name, "-D", str(domains), "--no-iter", str(no_iters), "--count", str(count)]
    if validate:
        cmd += ["-T"]
    if init_count:
        cmd += ["--init-count", str(init_count)]
    if sorted:
        cmd += ["-s"]
    if no_searches:
        cmd += ["--no-searches", str(no_searches)]
    if search_threshold:
        cmd += ["--search-threshold", str(search_threshold)]
    if insert_threshold:
        cmd += ["--insert-threshold", str(insert_threshold)]
    if branching_factor:
        cmd += ["--branching-factor", str(branching_factor)]

    result = subprocess.run(cmd, capture_output=True)
    stdout = result.stdout.decode("utf-8").splitlines()
    for output in stdout[:-1]:
        print(output)
    [time, _, var] = stdout[-1].split()
    time = time.removesuffix("s").strip()
    var = var.removesuffix("s").strip()
    if verbose:
        print(f"time for {name} with {count} inserts was {time} +- {var}")
    return float(time), float(var)

def run_test(op, args):
    if isinstance(op, str):
        res = run_process(op, **args)
    elif isinstance(op, dict):
        op_args={key: op[key] for key in op if key not in {'name', 'label', 'title'}}
        res = run_process(op['name'], **op_args, **args)
    else:
        raise ValueError(f'Invalid operation {op}')
    return res

def test_name(op):
    if isinstance(op, str):
        return op
    elif isinstance(op, dict) and 'title' in op:
        return op['title']
    else:
        raise ValueError(f'Invalid operation {op}')

def test_label(op):
    if isinstance(op, str):
        return op
    elif isinstance(op, dict) and 'label' in op:
        return op['label']
    else:
        raise ValueError(f'Invalid operation {op}')

def build_results(data_structures, args, param='domains', values=None):
    results = []
    if not values:
        values = range(1, 16)
    for i in tqdm(values):
        result = {param: i}
        for data_structure in data_structures:
            time, sd = run_test(data_structure, {param:i, **args})
            name = test_label(data_structure)
            result[name] = time
            result[name + "-sd"] = sd
        results.append(result)
    return results

def plot_results(param, data_structures, results, title=None, xlabel=None):
    if not title:
        title = f"Comparison of {param} values on data structure"
    if not xlabel:
        xlabel=param
    param_values = [data[param] for data in results]
    fig = plt.figure(figsize=(12,8), dpi=100, facecolor='w', edgecolor='k')
    for data_structure in data_structures:
        label = test_label(data_structure)
        name = test_name(data_structure)
        values = [data[label] for data in results]
        err = [data[label + "-sd"] for data in results]
        plt.errorbar(param_values, values, yerr=err, label=name)
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel('Time (s)')
    plt.legend()
    plt.show() 

def interactive_plot():
    fig = plt.figure(figsize=(8,6), dpi=100, facecolor='w', edgecolor='k')
    ax = fig.add_subplot(111)
    plt.ion()
    return fig,ax

def build_interactive_plot(fig,ax, data_structures, params={}, title=None, xlabel=None, param=None, values=None):
    if not param:
        param = 'domains'
    if not values:
        values = range(1, 16)
    if not title:
        title = f"Comparison on value of {param} on data structure"
    if not xlabel:
        xlabel = param

    times = []
    results = []
    for i in values:
        result={param: i}
        times.append(i)
        results.append(result)
        for data_structure in data_structures:
            t,var = run_test(data_structure,{param: i, **params})
            label = test_label(data_structure)
            result[label]=t
            result[label+'-sd']=var

            ax.clear()
            ax.set_title(title)
            ax.set_xlabel(xlabel)
            ax.set_ylabel('Time (s)')
            for data_structure in data_structures:
                label = test_label(data_structure)
                name = test_name(data_structure)
                available_values=[data[label] for data in results if label in data]
                available_var=[data[label+'-sd'] for data in results if (label+'-sd') in data]
                available_times=times[:len(available_values)]
                ax.errorbar(available_times, available_values, yerr=available_var, label=name)
            ax.legend()
            fig.canvas.draw()
    return times, results
