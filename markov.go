package main

import "fmt"
import "github.com/skelterjohn/go.matrix"
import "math/rand"

// Ergodic Markov matrix
var ErgodicMat = matrix.MakeDenseMatrixStacked([][]float64{
	[]float64{0.1, 0.4, 0.5},
	[]float64{0.3, 0.6, 0.1},
	[]float64{0.2, 0.5, 0.3}})

// Absorbing Markov matrix
var AbsorbingMat = matrix.MakeDenseMatrixStacked([][]float64{
	[]float64{1, 0, 0, 0},
	[]float64{0.1, 0.3, 0.3, 0.3},
	[]float64{0.2, 0.4, 0.3, 0.1},
	[]float64{0, 0, 0, 1}})

// Transient Mat
var TransMat = matrix.MakeDenseMatrix([]float64{0.3, 0.3, 0.4, 0.3}, 2, 2)

// Absorbing state
var TrapMat = matrix.MakeDenseMatrix([]float64{0.1, 0.3, 0.2, 0.1}, 2, 2)

func main() {
	case1 := func() {
		V, _, err := ErgodicMat.Transpose().Eigen()
		pi := matrix.MakeDenseMatrixStacked(unify(V.GetColVector(0).Arrays()))
		M := ErgodicMat.Copy()
		for i := 0; i < 10; i++ {
			M, _ = M.TimesDense(M)
		}
		if err == nil {
			fmt.Printf("\nCASE 1: Test using given matrices\n\n Ergodic Matrix:\n%s\n\n Absorbing Matrix:\n%s\n\n Steady state equilibrium for ergodic chain:\n%s\n\n Verify by matrix multiplication:\n%s\n\n Verify by simulation(500 tests, each run 1000 steps, init prob [0.3,0.3,0.4]):\n%v\n\n Absorbing probabilities matrix(unified) using Chapman–Kolmogorov equation:\n%s\n\n Verify by simulation(500 tests):\n%s\n", ErgodicMat, AbsorbingMat, pi, M, statErgodicAtStepN(500, 1000, []float64{0.3, 0.3, 0.4}), absorbingStates(), statAbsorbing(500))

		}
	}

	case2 := func() {
		V, _, err := ErgodicMat.Transpose().Eigen()
		pi := matrix.MakeDenseMatrixStacked(unify(V.GetColVector(0).Arrays()))
		M := ErgodicMat.Copy()
		for i := 0; i < 10; i++ {
			M, _ = M.TimesDense(M)
		}
		if err == nil {
			fmt.Printf("\nCASE 2: Test using given matrices\n\n Ergodic Matrix:\n%s\n\n Absorbing Matrix:\n%s\n\n Steady state equilibrium for ergodic chain:\n%s\n\n Verify by matrix multiplication:\n%s\n\n Verify by simulation(2000 tests, each run 3000 steps, init prob [0.8,0.1,0.1]):\n%v\n\n Absorbing probabilities matrix(unified) using Chapman–Kolmogorov equation:\n%s\n\n Verify by simulation(1000 tests):\n%s\n", ErgodicMat, AbsorbingMat, pi, M, statErgodicAtStepN(2000, 3000, []float64{0.8, 0.1, 0.1}), absorbingStates(), statAbsorbing(2000))

		}
	}

	case3 := func() {
		ErgodicMat = matrix.MakeDenseMatrixStacked([][]float64{
			[]float64{0.2, 0.3, 0.5},
			[]float64{0.2, 0.6, 0.2},
			[]float64{0.2, 0.5, 0.3}})

		AbsorbingMat = matrix.MakeDenseMatrixStacked([][]float64{
			[]float64{1, 0, 0, 0},
			[]float64{0.2, 0.2, 0.3, 0.3},
			[]float64{0.4, 0.1, 0.3, 0.2},
			[]float64{0, 0, 0, 1}})

		TransMat = matrix.MakeDenseMatrix([]float64{0.2, 0.3, 0.1, 0.3}, 2, 2)

		TrapMat = matrix.MakeDenseMatrix([]float64{0.2, 0.3, 0.4, 0.2}, 2, 2)

		V, _, err := ErgodicMat.Transpose().Eigen()
		pi := matrix.MakeDenseMatrixStacked(unify(V.GetColVector(1).Arrays()))
		M := ErgodicMat.Copy()
		for i := 0; i < 10; i++ {
			M, _ = M.TimesDense(M)
		}
		if err == nil {
			fmt.Printf("\nCASE 3: Test using other matrices\n\n Ergodic Matrix:\n%s\n\n Absorbing Matrix:\n%s\n\n Steady state equilibrium for ergodic chain:\n%s\n\n Verify by matrix multiplication:\n%s\n\n Verify by simulation(2000 tests, each run 4000 steps, init prob [0.3,0.3,0.4]):\n%v\n\n Absorbing probabilities matrix(unified) using Chapman–Kolmogorov equation:\n%s\n\n Verify by simulation(10000 tests):\n%s\n", ErgodicMat, AbsorbingMat, pi, M, statErgodicAtStepN(2000, 4000, []float64{0.3, 0.3, 0.4}), absorbingStates(), statAbsorbing(10000))

		}
	}

	case3()
	if 1 == 0 {
		case1()
		case2()
		case3()
	}
}

// unify FLAT vector
func unify(vec [][]float64) [][]float64 {
	ret := make([][]float64, len(vec))
	sum := 0.0
	for _, val := range vec {
		sum += val[0]
	}

	for i := range ret {
		ret[i] = []float64{vec[i][0] / sum}
	}
	return ret
}

func cumProbMat(M *matrix.DenseMatrix) *matrix.DenseMatrix {
	C := M.Copy()
	rows := C.Rows()
	cols := C.Cols()
	for i := 0; i < rows; i++ {
		for j := 1; j < cols; j++ {
			C.Set(i, j, C.Get(i, j)+C.Get(i, j-1))
		}
	}
	return C
}

// stat states at step N
func statErgodicAtStepN(size, N int, prob []float64) []float64 {
	statCnt := make([]int, ErgodicMat.Rows())
	cumProb := make([]float64, len(prob))
	cumMat := cumProbMat(ErgodicMat)

	copy(cumProb, prob)
	for i := 1; i < len(cumProb); i++ {
		cumProb[i] += cumProb[i-1]
	}

	for i := 0; i < size; i++ {
		stat := 0
		r := rand.Float64()
		for k, v := range cumProb {
			if r < v {
				stat = k
				break
			}
		}

		for j := 0; j < N; j++ {
			row := cumMat.GetRowVector(stat)
			r := rand.Float64()
			for k := 0; k < row.Cols(); k++ {
				if r < row.Get(0, k) {
					stat = k
					break
				}
			}
		}

		statCnt[stat]++
	}

	statProbCnt := make([]float64, len(statCnt))
	sum := 0.0
	for _, v := range statCnt {
		sum += float64(v)
	}

	for k := range statProbCnt {
		statProbCnt[k] = float64(statCnt[k]) / sum
	}
	return statProbCnt
}

// return absorbing states indices if any.
func findAbsorbing(M *matrix.DenseMatrix) []int {
	var idx []int

	for i, rows, cols := 0, M.Rows(), M.Cols(); i < rows; i++ {
		for j := 0; j < cols; j++ {
			if 1 == M.Get(i, j) {
				idx = append(idx, i)
				continue
			}
		}
	}
	return idx
}

func isTrapped(i int, a []int) bool {
	for _, v := range a {
		if i == v {
			return true
		}
	}
	return false
}

func absorbingStates() *matrix.DenseMatrix {
	chk := func(e error) {
		if e != nil {
			panic(e)
		}
	}

	rows := TransMat.Rows()

	min, err := matrix.Eye(rows).MinusDense(TransMat)
	chk(err)
	inv, err := min.Inverse()
	chk(err)
	res, err := inv.TimesDense(TrapMat)
	chk(err)

	return unifyMat(res)
}

func unifyMat(M *matrix.DenseMatrix) *matrix.DenseMatrix {
	c := M.Copy()

	for i := 0; i < c.Rows(); i++ {
		rowSum := 0.0
		for j := 0; j < c.Cols(); j++ {
			rowSum += M.Get(i, j)
		}
		for j := 0; j < c.Cols(); j++ {
			c.Set(i, j, M.Get(i, j)/rowSum)
		}
	}
	return c
}

func statAbsorbing(size int) *matrix.DenseMatrix {

	cumAbsorbMat := cumProbMat(AbsorbingMat)
	res := [][]float64{[]float64{0, 0}, []float64{0, 0}}

	for _, i := range []int{1, 2} {
		for n := 0; n < size; n++ {
			stat := i
			for !isTrapped(stat, []int{0, 3}) {
				r := rand.Float64()
				for k := 0; k < 4; k++ {
					if r < cumAbsorbMat.Get(stat, k) {
						stat = k
						break
					}
				}
			}

			if stat == 0 {
				res[i-1][0]++
			} else {
				res[i-1][1]++
			}
		}
		res[i-1][0] /= float64(size)
		res[i-1][1] /= float64(size)
	}

	return matrix.MakeDenseMatrixStacked(res)
}
