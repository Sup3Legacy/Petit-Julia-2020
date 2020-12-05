function ackerman(m::Int64, n::Int64)
	if m == 0 (n + 1)
	elif 0 == n ackerman(m-1, 1)
	else ackerman(m-1; ackerman(m; n-1)) end
end
