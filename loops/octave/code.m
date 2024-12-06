function code()
    args = argv();  % Parse input arguments
    u = str2double(args{1});  % Convert first input argument to double

    r = randi([0, 10000]);  % Get a random number 0 <= r < 10k

    mod_array = mod(0:99999, u);  % Calculate j % u
    inner_sum = sum(mod_array);  % Sum the values

    a = repmat(inner_sum, 1, 10000);  % Initialize array of 10k elements to inner_sum

    a = a + r; % Add the random value to each element

    disp(a(r+1));  % display result (add +1 to maintain same behavior as Python)
end

code();
