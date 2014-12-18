package main

import "math/rand"
import "fmt"
import "math"

var pi = []float64{0.1, 0.2, 0.3, 0.05, 0.2, 0.1, 0.05}
var po = []float64{0.15, 0.05, 0.15, 0.25, 0.05, 0.2, 0.15}

var k int
var cumPi []float64
var cumPo []float64

func init() {
	k = len(pi)
	if k != len(po) {
		panic(k)
	}
	cumPi = make([]float64, k)
	cumPo = make([]float64, k)

	sum := 0.0
	for i, v := range pi {
		sum += v
		cumPi[i] = sum
	}

	sum = 0
	for i, v := range po {
		sum += v
		cumPo[i] = sum
	}
}

func cumVec(p []float64) []float64 {
	sum := 0.0
	cum := make([]float64, len(p))
	for i, v := range p {
		sum += v
		cum[i] = sum
	}
	return cum
}

// get a sample based on cum vec
func sample(cum []float64) int {
	r := rand.Float64()
	for i := range cum {
		if r < cum[i] {
			return i
		}
	}
	panic(r)
	return len(cum)
}

func natural() float64 {
	n := 10000
	sum := 0.0
	for i := 0; i < n; i++ {
		o, i := eventOI()
		sum += T(o, i, rand.Intn(k))
	}
	return sum / float64(n)
}

func eventOI() (int, int) {
	return sample(cumPo), sample(cumPi)
}

//func tsetlin(N, )

func tsetlinNext(N, R, i int, pen bool) int {
	//penalty
	if pen {
		ni := i + 1
		if 0 == ni%N {
			return (i + N) % (R * N)
		}
		return ni
	}
	//reward
	if 0 == i%N {
		return i
	}
	return i - 1
}

func T(o, i, a int) float64 {
	if o > k || i > k || a > k {
		panic(a)
	}
	fo := float64(o)
	fi := float64(i)
	fa := float64(a)
	return 0.5*math.Abs(fo-fa) + math.Abs(fa-fi)
}

func simuTsetlin(N, times int) []float64 {
	X := make([]int, k)
	for i := range X {
		X[i] = rand.Intn(N * k)
	}
	log := make([]float64, times)

	for j := 0; j < times; j++ {

		o, i := eventOI()
		t := T(o, i, X[o]/N)
		log[j] = t

		tsum := 0.0
		for sa := 0; sa < k; sa++ {
			for si := 0; si < k; si++ {
				tsum += T(o, si, sa)
			}
		}

		p := false
		if rand.Float64() < t/tsum {
			p = true
		}
		X[o] = tsetlinNext(N, k, X[o], p)
	}
	return log
}

func avg(sl []float64) float64 {
	sum := 0.0
	for _, v := range sl {
		sum += v
	}
	return sum / float64(len(sl))
}

func avgPlot(log []float64, width, step int) []float64 {
	ss := (len(log) - width) / step
	points := make([]float64, ss)
	for i := 0; i < ss; i++ {
		points[i] = avg(log[i*step : i*step+width])
	}
	return points
}

func best() float64 {
	e := 0.0
	for o := 0; o < k; o++ {
		// find min mu
		mint := math.Inf(1)
		for a := 0; a < k; a++ {
			t := 0.0
			for i := 0; i < k; i++ {
				t += T(o, i, a) * pi[i]
			}
			if t < mint {
				mint = t
			}
		}
		e += mint * po[o]
	}
	return e
}

func simubest() float64 {
	besta := make([]int, k)
	for o := 0; o < k; o++ {
		// find min mu
		mint := math.Inf(1)
		for a := 0; a < k; a++ {
			t := 0.0
			for i := 0; i < k; i++ {
				t += T(o, i, a) * pi[i]
			}
			if t < mint {
				besta[o] = a
				mint = t
			}
		}
	}

	e := 0.0
	n := 100000
	for i := 0; i < n; i++ {
		o, i := eventOI()
		e += T(o, i, besta[o])
	}
	e /= float64(n)
	return e
}

func allavg(log func() []float64, ins, cut int) float64 {
	sum := 0.0
	for i := 0; i < ins; i++ {
		dat := log()
		sum += avg(dat[len(dat)-cut:])
	}
	return sum / float64(ins)
}

func simuEst(times int) []float64 {
	epi := make([]float64, k)
	alpha := make([][]float64, k)
	cnt := make([]int, k)
	ak := 0.1
	log := make([]float64, times)

	for i := range cnt {
		cnt[i] = 1
		alpha[i] = make([]float64, k)
		for j := range alpha[i] {
			alpha[i][j] = 1.0 / float64(k)
		}
	}

	updateepi := func(ci int) {
		sum := 0.0
		cnt[ci]++
		for _, v := range cnt {
			sum += float64(v)
		}
		for i := range cnt {
			epi[i] = float64(cnt[i]) / sum
		}
	}

	updatealpha := func(co, ba int) {
		temp := make([]float64, k)
		temp[ba] = ak
		for i, v := range alpha[co] {
			alpha[co][i] = (1-ak)*v + temp[i]
		}
	}

	choose := func(co int) int {
		return sample(cumVec(alpha[co]))
	}

	for m := 0; m < times; m++ {
		o, i := eventOI()
		updateepi(i)
		besta := 0
		// find min mu
		mint := math.Inf(1)
		for a := 0; a < k; a++ {
			t := 0.0
			for i := 0; i < k; i++ {
				t += T(o, i, a) * epi[i]
			}
			if t < mint {
				besta = a
				mint = t
			}
		}
		updatealpha(o, besta)
		a := choose(o)
		log[m] = T(o, i, a)
	}
	return log
}

func main() {
	fmt.Printf("Theory Best: (%v)\n", best())
	fmt.Printf("Simulate Theory Best(%v)\n", simubest())
	fmt.Printf("Random Select (%v)\n", natural())
	fmt.Printf("Simulate Estimator (%v)\n", avg(simuEst(50000)[1000:]))
	fmt.Printf("50-state Tsetlin (%v)\n", avg(simuTsetlin(50, 1000000)[100000:]))
	//fmt.Printf("(%v)\n", allavg(func() []float64 { return simuTsetlin(4, 100000) }, 10, 1000))
}
