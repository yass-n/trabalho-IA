import ExpertSys.Tipos
import ExpertSys.Match
import ExpertSys.Unify
import ExpertSys.Stream
import ExpertSys.Parser
import ExpertSys.Expert
import Control.Monad.State
import Data.Maybe
import System.Exit

main:: IO ()
main = do

	putStrLn "Sistema Expecialista de sugestoes de relacionamento"
	let e str = ex where (ex,_):_ = parse expr str

	--var. utilizadas pra construir as regras
	let pessoa1PrefAtributo1 = e "(? pessoa1) prefere (? atributo1)"
	let pessoa2TemAtributo1 = e "(? pessoa2) tem (? atributo1)"

	let pessoa1GostarPessoa2 = e "(? pessoa1) pode gostar de (? pessoa2)"
	let pessoa2GostarPessoa1 = e "(? pessoa2) pode gostar de (? pessoa1)"

	let pessoa1AmigoPessoa2 = e "(? pessoa1) pode ser amigo de (? pessoa2)"
	let pessoa2AmigoPessoa1 = e "(? pessoa2) pode ser amigo de (? pessoa1)"

	let pessoa1ProcuraSexo1 = e "(? pessoa1) procura (? sexo1)"
	let pessoa2ProcuraSexo2 = e "(? pessoa2) procura (? sexo2)"

	let pessoa1ehSexo2 = e "(? pessoa1) eh (? sexo2)"
	let pessoa2ehSexo1 = e "(? pessoa2) eh (? sexo1)"

	let pessoa1NamorarPessoa2 = e "(? pessoa1) pode namorar (? pessoa2)"

	--var. utilizadas pra construir afirmacoes

	let sandra1 = e "(Sandra prefere olhos-verdes)"
	let sandra2 = e "(Sandra tem olhos-castanhos)"
	let sandra3 = e "(Sandra eh Mulher)"
	let sandra4 = e "(Sandra procura Homem)"

	let patricia1 = e "(Patricia prefere olhos-castanhos)"
	let patricia2 = e "(Patricia tem olhos-verdes)"
	let patricia3 = e "(Patricia eh Mulher)"
	let patricia4 = e "(Patricia procura Mulher)"

	let joaquim1 = e "(Joaquim prefere olhos-castanhos)"
	let joaquim2 = e "(Joaquim tem olhos-verdes)"
	let joaquim3 = e "(Joaquim eh Homem)"
	let joaquim4 = e "(Joaquim procura Mulher)"

	let joao1 = e "(Joao prefere olhos-verdes)"
	let joao2 = e "(Joao tem olhos-castanhos)"
	let joao3 = e "(Joao eh Homem)"
	let joao4 = e "(Joao procura Homem)"

	-- Regras

	let regra1 = Rule "preferencia1"
				[pessoa1PrefAtributo1, 
				pessoa2TemAtributo1]
				pessoa1GostarPessoa2

	let regra2 = Rule "preferencia2"
				[pessoa1GostarPessoa2,
				pessoa2GostarPessoa1]
				pessoa1AmigoPessoa2

	let regra3 = Rule "preferencia3"
				[pessoa1AmigoPessoa2,
				pessoa2AmigoPessoa1,
				pessoa1ProcuraSexo1,
				pessoa2ProcuraSexo2,
				pessoa1ehSexo2,
				pessoa2ehSexo1]
				pessoa1NamorarPessoa2

	-- Afirmacoes

	let sandra = (Stream sandra1
					(Stream sandra2
						(Stream sandra3
							(Stream sandra4 EmptyStream))))

	let patricia = (Stream patricia1
						(Stream patricia2
							(Stream patricia3
								(Stream patricia4 EmptyStream))))

	let joaquim = (Stream joaquim1
						(Stream joaquim2
							(Stream joaquim3
								(Stream joaquim4 EmptyStream))))

	let joao = (Stream joao1
					(Stream joao2
						(Stream joao3
							(Stream joao4 EmptyStream))))

	let streams = (Stream sandra
						(Stream patricia
							(Stream joaquim
								(Stream joao EmptyStream))))

	let assertion = streamConcatenate streams


	let rules = (Stream regra1 
					(Stream regra2 
						(Stream regra3 EmptyStream)))

	let kb = Kb assertion rules

	runStateT forwardChain kb

	return ()