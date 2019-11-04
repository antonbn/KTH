defmodule Huffman do

    def sample do
        'the quick brown fox jumps over the lazy dog
        this is a sample text that we will use when we build
        up a table we will only handle lower case letters and
        no punctuation symbols the frequency will of course not
        represent english but it is probably not that far off'
    end

    def text() do
        'this is something that we should encode'
    end

    def test do
        sample = sample()
        tree = tree(sample)
        encode = encode_table(tree)
        decode = decode_table(tree)
        text = text()
        seq = encode(text, encode)
        decode(seq, decode)
    end
    
    def insert(tuple, []), do: [tuple]
    def insert({c1, f1}, [{c2, f2} | tail]) do
        cond do
            f1 > f2 -> [{c1, f1}, {c2, f2} | tail]
            true -> [{c2, f2} | insert({c1, f1}, tail)]
        end
    end

    def isort(l), do: isort(l, [])
    def isort([], sorted), do: sorted
    def isort([head | tail], sorted) do
        isort(tail, insert(head, sorted))
    end

    def update_freq(char, []), do: [{char, 1}]
    def update_freq(char, [{char, freq} | rest]) do
        [{char, freq + 1} | rest]
    end
    def update_freq(char, [tuple | rest]) do
        [tuple | update_freq(char, rest)]
    end

    def freq(sample), do: freq(sample, [])
    def freq([], freq), do: freq
    def freq([char | rest], freq) do
        freq(rest, update_freq(char, freq))
    end

    def combine_last([{c1, f1}, {c2, f2}]), do: [{{c1, c2}, f1 + f2}]
    def combine_last([head | tail]), do: [head | combine_last(tail)]

    def huffman([{tree, _}]), do: tree
    def huffman(freq) do
        sorted_freq = isort(freq)
        combined_freq = combine_last(sorted_freq)
        huffman(combined_freq)
    end

    def tree(sample) do
        freq = freq(sample)
        huffman(freq)
    end

    def encode_table({left, right}, path) do
        left_paths = encode_table(left, path ++ [0])
        right_paths = encode_table(right, path ++ [1])
        left_paths ++ right_paths
    end
    def encode_table(leaf, path), do: [{leaf, path}]
    def encode_table(tree), do: encode_table(tree, [])
    
    def decode_table(tree), do: encode_table(tree, [])

    def get_encoding(char, [{char, encoding} | _]), do: encoding
    def get_encoding(char, [_ | tail]), do: get_encoding(char, tail)

    def encode([], _), do: []
    def encode([char | rest], table) do
        encoding = get_encoding(char, table)
        encoding ++ encode(rest, table)
    end
    
    def decode_char(seq, n, table) do
        {code, rest} = Enum.split(seq, n)
        case List.keyfind(table, code, 1) do
            {char, _} -> {char, rest}
            nil -> decode_char(seq, n + 1, table)
        end
    end

    def decode([], _), do: []
    def decode(seq, table) do
        {char, rest} = decode_char(seq, 1, table)
        [char | decode(rest, table)]
    end

    def read(file, n) do
        {:ok, file} = File.open(file, [:read])
        binary = IO.read(file, n)
        File.close(file)
        case :unicode.characters_to_list(binary, :utf8) do
            {:incomplete, list, _} -> list
            list -> list
        end
    end
end