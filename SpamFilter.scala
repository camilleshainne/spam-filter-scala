// Compile: scalac SpamFilter.scala
// Run: scala SpamFilter [input_file].txt [spam_data].txt [ham_data].txt

import java.io._
import java.lang._
import java.util._
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control._

object SpamFilter{
	def main(args: Array[String]){
		var spam = args(1)	// second argument for spam training data
		var ham = args(2)	// third argument for ham training data
		var spamfilter = new SpamFilter(spam, ham)

		spamfilter.filterSpam(new File(args(0))) // first argument for input file
		spamfilter.filter()
		spamfilter.saveOutput()
	}

	class SpamFilter(spamFile: String, hamFile: String) {
		var spam = new BagOfWords(spamFile)
		var ham = new BagOfWords(hamFile)
		var message = new Hashtable[String, Double]();
		var category = new Hashtable[String, String]()
		var totalSize : Int = 0
		var totalLines : Int = 0
		var k : Int = 0

		totalSize = this.getDictionarySize()		// total dictionary size for spam and ham bag-of-words
		totalLines = spam.lines + ham.lines		// total message lines of spam and ham bag-of-words

		def getDictionarySize() : Int = {
			var dictionarySize : Int = spam.dictionarySize
			var key = new String

			for(key <- ham.bagOfWords.keySet()){
				if(!spam.bagOfWords.containsKey(key)) dictionarySize += 1
			}
			
			return dictionarySize
		}

		def filterSpam(file: File) {
			var input : ArrayList[String] = this.loadFile(file)
			var m = new String

			var pWs : Double = 1.0		// P(message|spam)
			var pWh : Double = 1.0		// P(message|ham)
			var pSpam : Double = ((spam.lines + this.k).toDouble) / ((totalLines + (2 * k)).toDouble)	// P(spam)
			var pHam : Double = ((ham.lines + this.k).toDouble) / ((totalLines + (2 * k)).toDouble)		// P(ham)
			var pSm : Double = 0.0		// P(spam|message)
			var msg = new String

			for(msg <- input){
				var num : Int = 0
				var string : Array[String] = msg.split(" ")

				for(m <- string){
					if(!spam.bagOfWords.containsKey(m) && !ham.bagOfWords.containsKey(m)) num += 1
				}

				for(m <- string){
					var s : Int = 0
					var h : Int = 0

					var sp = new java.lang.Integer(spam.bagOfWords.get(m))
					var ha = new java.lang.Integer(ham.bagOfWords.get(m))

					if(sp == null) s = 0		// checks if word is in the spam bag-of-words
					else s = sp
					pWs *= (s + this.k).toDouble / (spam.totalWords + (this.k * (totalSize + num))).toDouble

					if(ha == null) h = 0		// checks if word is in the ham bag-of-words
					else h = ha
					pWh *= (h + this.k).toDouble / (ham.totalWords + (this.k * (totalSize + num))).toDouble
				}

				pSm = (pWs * pSpam) / ((pWs * pSpam) + (pWh * pHam)) // P(spam|message)
				message.put(msg, pSm)
			}
		}

		def loadFile(file: File) : ArrayList[String] = {
			var msg = new ArrayList[String]()
			val loop = new Breaks

			try{
				var read = new FileReader(file)
				var buffer = new BufferedReader(read)
				var word = buffer.readLine()
				this.k = java.lang.Integer.parseInt(word)

				loop.breakable{
					while(word != null){
						word = buffer.readLine()
						if(word == null) loop.break
						word = word.replaceAll("[^a-zA-Z0-9 ]", "")
						msg.add(word.toLowerCase())
					}
				}

				buffer.close()
			} catch {
				case ex: FileNotFoundException => {
					println("Missing file exception")
					return null
				}
				case ex: IOException => {
					println("IOException")
					return null
				}
			}

			return msg
		}

		def filter() {
			var key = new String

			for(key <- this.message.keySet()){
				if(this.message.get(key) > 0.5) this.category.put(key, "SPAM")	// categorizes message as spam
				else this.category.put(key, "HAM")				// categorizes message as ham
			}
		}

		def saveOutput() : Boolean = {
			try{
				var file = new File("output.txt")

				if(!file.exists()) file.createNewFile()

				var writer = new FileWriter(file)
				var buffer = new BufferedWriter(writer)
				var key = new String

				for(key <- this.category.keySet()) buffer.write(key + " - " + this.category.get(key) + "\n")
				
				buffer.close()
				true
			} catch {
				case ex: FileNotFoundException => {
					println("Missing file exception")
					false
				}
				case ex: IOException => {
					println("IOException")
					false
				}
			}
		}
	}

	class BagOfWords(input: String){
		var bagOfWords = new Hashtable[String, Int]()
		var words = new ArrayList[String]()
		var lines : Int = _
		var dictionarySize : Int = _
		var totalWords : Int = _

		try {
			loadWords(new File(input))
		} catch {
			case ex: FileNotFoundException => {
				println("Missing file exception")
				false
			}
			case ex: IOException => {
				println("IOException")
				false
			}
		}

		bagOfWords = listWords(words)
		dictionarySize = bagOfWords.keySet().size()
		totalWords = words.size()

		def loadWords(file: File): Boolean = {
			try{
				var scanner = new Scanner(file)

				while(scanner.hasNext()){
					var addWord = scanner.next
					addWord = addWord.replaceAll("[^a-zA-Z0-9 ]", "")
					words.add(addWord.toLowerCase())
				}

				scanner.close()
			} catch {
				case ex: FileNotFoundException => {
					println("Missing file exception")
					false
				}
				case ex: IOException => {
					println("IOException")
					false
				}
			}

			try {
				var read = new FileReader(file)
				var buffer = new BufferedReader(read)

				while(buffer.readLine() != null) lines += 1
				
				buffer.close()
			} catch {
				case ex: FileNotFoundException => {
					println("Missing file exception")
					false
				}
				case ex: IOException => {
					println("IOException")
					false
				}
			}

			true
		}

		def listWords(w: ArrayList[String]) : Hashtable[String, Int] = {
			var table = new Hashtable[String, Int]()
			var s = new String

			for(s <- w){
				if(table.containsKey(s)) table.put(s, table.get(s)+1)
				else table.put(s, 1) 	// new word
			}

			return table
		}
	}
}
