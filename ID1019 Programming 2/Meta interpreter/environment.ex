defmodule Env do
    
    def new(), do: []

    def add(id, str, env) do
        [{id, str} | env]
    end

    def lookup(id, env) do
        case env do
            [] -> nil
            [{id, str} | _] -> {id, str}
            [_ | t] -> lookup(id, t)
        end
    end

    def remove(ids, env), do: remove(ids, ids, env)
    def remove(_, _, []), do: []
    def remove(ids, [], [env_h | env_t]), do: [env_h | remove(ids, ids, env_t)]
    def remove(ids, [id | _], [{id, _} | env_t]), do: remove(ids, ids, env_t)
    def remove(ids, [_ | id_t], env), do: remove(ids, id_t, env)

end