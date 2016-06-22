import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object SpamFilter{
	def main(args: Array[String]){
		var spam = new BagOfWords(args(0))	// second argument for spam training data
		var ham = new BagOfWords(args(1))	// third argument for ham training data
		var message : Map[String, Double] = Map()
		var totalSize : Int = 0
		var totalLines : Int = 0
		var k : Int = 0

		totalSize = getDictionarySize(new File(args(0)), new File(args(1)))
		totalLines = spam.lines + ham.lines

		println("Total Dictionary Size: " + totalSize)
		println("Total Number of Lines: " + totalLines)
	}

	def getDictionarySize(spam: File, ham: File) : Int = {
		var size : Int = 0
		var table : Map[String, Int] = Map()
		var table1 : Map[String, Int] = Map()
		var table2 : Map[String, Int] = Map()
		var s = Source.fromFile(spam).mkString
		var h = Source.fromFile(ham).mkString

		s = s.toLowerCase().replaceAll("[^a-zA-Z0-9\\s]", "")
		table1 = s.split("\\s+").groupBy(x=>x).mapValues(x=>x.length)
		h = h.toLowerCase().replaceAll("[^a-zA-Z0-9\\s]", "")
		table2 = h.split("\\s+").groupBy(x=>x).mapValues(x=>x.length)

		table = table1 ++ table2
		size = table.size

		return size
	}

	def filterSpam(file: File) {

	}

	def loadFile(file: File) : ArrayBuffer{

	}

	class BagOfWords(input: String){
		var bagOfWords : Map[String,Int] = Map()
		var words = new ArrayBuffer()
		var bag = new ArrayBuffer()
		var lines : Int = _
		var dictionarySize : Int = _
		var totalWords : Int = _

		bagOfWords = listWords(new File(input))

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

		dictionarySize = bagOfWords.size
		for(sum <- bagOfWords.values) totalWords += sum
		println("Dictionary Size: " + dictionarySize)
		println("Total Words: " + totalWords)
		println(listWords(new File(input)))

		def loadWords(file: File): Boolean = {

			try {
				for(line <- Source.fromFile(file).getLines) lines += 1
			}
			catch {
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

		def listWords(file: File) : Map[String, Int] = {
			var table : Map[String, Int] = Map()

			var addWord = Source.fromFile(file).mkString // returns the file as String
			addWord = addWord.toLowerCase().replaceAll("[^a-zA-Z0-9\\s]", "")
			table = addWord.split("\\s+").groupBy(x=>x).mapValues(x=>x.length)

			return table
		}
	}
}