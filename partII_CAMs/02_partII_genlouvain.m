addpath('GenLouvain');
addpath('GenLouvain/HelperFunctions');

load('outputs/01_dataPreperation/final/CAMaggregated_adj_matrices.mat');

multigraph_adj_matrices_list = struct2cell(multigraph_adj_matrices_list);

N = 33;
T = length(multigraph_adj_matrices_list);

omega = 0:0.1:1;

results = cell(length(omega), 1);

for i = 1:length(omega)
    [B,twom] = multicat(multigraph_adj_matrices_list, 1, omega(i));

    [S,Q,n_it] = iterated_genlouvain(B, 10000, 0, 1, 'moverandw');

    S = reshape(S, N, T);

    results{i} = {S, Q, n_it, omega(i)}
end

save("outputs/genlouvain_results.mat", "results")