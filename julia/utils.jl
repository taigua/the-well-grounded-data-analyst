using FreqTables
using PrettyTables
using DataFrames

function value_counts(df::DataFrame, field::Symbol; skipmissing=false, normalize=false, ordered=false, rev=false, n=0)
    ft = freqtable(df[!, field], skipmissing=skipmissing)
    if normalize
        ft = prop(ft)
    end

    if ordered
        ft = sort(ft, rev=rev)
    end

    if n > 0
        ft = ft[1:n]
    end

    pretty_table(HTML, (field => names(ft, 1), count = vec(ft)))
end
