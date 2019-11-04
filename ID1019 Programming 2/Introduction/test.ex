defmodule Test do
    # Compute the double of a number.
    def double(n) do
        n * 2
    end

    def far_to_cel(degree) do
        (degree - 32) / 1.8
    end

    def area_rect(w, h) do
        w * h
    end

    def area_square(w) do
        area_rect(w, w)
    end

    def area_circ(r) do
        :math.pi() * r * r
    end

    def product(m, n) do
        if m == 0 do
            0
        else 
            n + product(m - 1, n)
        end
    end

    def exp(_, 0), do: 1
    def exp(x, n) do
        product(x, exp(x, n - 1))
    end

    def fast_exp(x, n) do
        cond do
            n == 0 -> 1
            n == 1 -> x
            rem(n, 2) == 0 -> 
                y = fast_exp(x, div(n, 2))
                y * y
            rem(n, 2) != 0 -> x * fast_exp(x, n - 1)
        end
    end

    def nth(n, [head | tail]) do
        case n do
            0 -> head
            _ -> nth(n - 1, tail)    
        end
    end

    def len([]), do: 0
    def len([_ | tail]) do
        1 + len(tail)
    end

    def sum([]), do: 0
    def sum([head | tail]) do
        head + sum(tail)
    end

    def duplicate([]), do: []
    def duplicate([head | tail]) do
        [head, head | duplicate(tail)]
    end

    def add(x, []), do: [x]
    def add(x, [head | tail]) do
        if x == head do
            [head | tail]
        else
            [head | add(x, tail)]
        end
    end

    def remove(_, []), do: []
    def remove(x, [head | tail]) do
        cond do
            x == head -> remove(x, tail)
            true -> [head | remove(x, tail)]
        end
    end

    def unique([head | tail]), do: unique([head], tail)
    def unique(unique_list, []), do: unique_list
    def unique(unique_list, [head | tail]) do
        if Enum.member?(unique_list, head) do
            unique(unique_list, tail)
        else
            unique([head | unique_list], tail)
        end
    end

    def sub_pack(_, []), do: []
    def sub_pack(x, [head | tail]) do
        cond do
            x == head -> [head | sub_pack(x, tail)]
            true -> sub_pack(x, tail)
        end
    end
    def pack([]), do: []
    def pack([head | tail]) do
        sub_pack = sub_pack(head, [head | tail])
        new_list = remove(head, [head | tail])
        [sub_pack | pack(new_list)]
    end

    def reverse([]), do: []
    def reverse([head | tail]) do
        reverse(tail) ++ [head]
    end

    def insert(element, []), do: [element]
    def insert(element, [head | tail]) do
        cond do
            element < head -> [element, head | tail]
            true -> [head | insert(element, tail)]
        end
    end

    def isort(l), do: isort(l, [])
    def isort([], sorted), do: sorted
    def isort([head | tail], sorted) do
        isort(tail, insert(head, sorted))
    end

    def msort([]), do: []
    def msort([h]), do: [h]
    def msort(l) do
        {l1, l2} = msplit(l, [], [])
        merge(msort(l1), msort(l2))
    end

    def merge(l, []), do: l
    def merge([], r), do: r
    def merge([h1 | t1], [h2 | t2]) do
        if h1 < h2 do
            [h1 | merge(t1, [h2 | t2])]
        else
            [h2 | merge([h1 | t1], t2)]
        end
    end

    def msplit([], l, r), do: {l, r}
    def msplit([h | t], l, r) do
        if length(l) < length(r) do
            msplit(t, [h | l], r)
        else
            msplit(t, l, [h | r])
        end
    end

    def qsort([]), do: []
    def qsort([h]), do: [h]
    def qsort([p | l]) do
        {l1, l2} = qsplit(p, l, [], [])
        small = qsort(l1)
        large = qsort(l2)
        append([p | large], small)
    end

    def qsplit(_, [], small, large), do: {small, large}
    def qsplit(p, [h | t], small, large) do
        if h < p do
            qsplit(p, t, [h | small], large)
        else
            qsplit(p, t, small, [h | large])
        end
    end

    def append(x, l) do
        case l do
            [] -> if is_list(x), do: x, else: [x]
            [h | t] -> [h | append(x, t)]
        end
    end

    def to_integer(x), do: to_integer(x, 0)
    def to_integer([], n), do: n
    def to_integer([x | r], n) do
        n0 = x * fast_exp(2, length(r)) + n
        to_integer(r, n0)
    end

    def fib(n), do: fib({0, 1}, n)
    def fib({f1, _}, 0), do: f1
    def fib({f1, f2}, n) do
        fib({f2, f1 + f2}, n - 1)
    end

end