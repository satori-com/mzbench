
function metrics()
    return {
        ['thingy'] = 'gauge',
        ['stuff'] = 'counter',
        ['konfabulator'] = 'histogram'
    }
end

function main()
    mzbench.notify('gauge', 'thingy', 42)
    mzbench.notify('histogram', 'konfabulator', 404)
    mzbench.notify('counter', 'stuff', 1)
end