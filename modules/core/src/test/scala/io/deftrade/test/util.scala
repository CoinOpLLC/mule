package io.deftrade

import eu.timepit.refined

object console {
  def slowrun[T, R](t: T)(run: T => R, zzz: Long = 100L): R =
    try run(t)
    finally Thread sleep zzz
}

package object test {

  val isins = List(
    "US0378331005",
    "US0373831005",
    "U50378331005",
    "US03378331005",
    "AU0000XVGZA3",
    "AU0000VXGZA3",
    "FR0000988040"
  )

  // from http://splasho.com/upgoer5/
  // @formatter:off
  val tenHundredWords = Vector(
    "a", "able", "about", "above", "accept", "across", "act", "actually", "add", "admit", "afraid",
    "after", "afternoon", "again", "against", "age", "ago", "agree", "ah", "ahead", "air", "all",
    "allow", "almost", "alone", "along", "already", "alright", "also", "although", "always", "am",
    "amaze", "an", "and", "anger", "angry", "animal", "annoy", "another", "answer", "any",
    "anymore", "anyone", "anything", "anyway", "apartment", "apparently", "appear", "approach",
    "are", "area", "aren't", "arm", "around", "arrive", "as", "ask", "asleep", "ass", "at",
    "attack", "attempt", "attention", "aunt", "avoid", "away", "baby", "back", "bad", "bag", "ball",
    "band", "bar", "barely", "bathroom", "be", "beat", "beautiful", "became", "because", "become",
    "bed", "bedroom", "been", "before", "began", "begin", "behind", "believe", "bell", "beside",
    "besides", "best", "better", "between", "big", "bit", "bite", "black", "blink", "block",
    "blonde", "blood", "blue", "blush", "body", "book", "bore", "both", "bother", "bottle",
    "bottom", "box", "boy", "boyfriend", "brain", "break", "breakfast", "breath", "breathe",
    "bright", "bring", "broke", "broken", "brother", "brought", "brown", "brush", "build", "burn",
    "burst", "bus", "business", "busy", "but", "buy", "by", "call", "calm", "came", "can", "can't",
    "car", "card", "care", "carefully", "carry", "case", "cat", "catch", "caught", "cause", "cell",
    "chair", "chance", "change", "chase", "check", "cheek", "chest", "child", "children", "chuckle",
    "city", "class", "clean", "clear", "climb", "close", "clothes", "coffee", "cold", "college",
    "color", "come", "comment", "complete", "completely", "computer", "concern", "confuse",
    "consider", "continue", "control", "conversation", "cool", "corner", "couch", "could",
    "couldn't", "counter", "couple", "course", "cover", "crack", "crazy", "cross", "crowd", "cry",
    "cup", "cut", "cute", "dad", "damn", "dance", "dark", "date", "daughter", "day", "dead", "deal",
    "dear", "death", "decide", "deep", "definitely", "desk", "did", "didn't", "die", "different",
    "dinner", "direction", "disappear", "do", "doctor", "does", "doesn't", "dog", "don't", "done",
    "door", "doubt", "down", "drag", "draw", "dream", "dress", "drink", "drive", "drop", "drove",
    "dry", "during", "each", "ear", "early", "easily", "easy", "eat", "edge", "either", "else",
    "empty", "end", "enjoy", "enough", "enter", "entire", "escape", "especially", "even", "evening",
    "eventually", "ever", "every", "everyone", "everything", "exactly", "except", "excite",
    "exclaim", "excuse", "expect", "explain", "expression", "eye", "eyebrow", "face", "fact",
    "fall", "family", "far", "fast", "father", "fault", "favorite", "fear", "feel", "feet", "fell",
    "felt", "few", "field", "fight", "figure", "fill", "finally", "find", "fine", "finger",
    "finish", "fire", "first", "fit", "five", "fix", "flash", "flip", "floor", "fly", "focus",
    "follow", "food", "foot", "for", "force", "forget", "form", "forward", "found", "four", "free",
    "friend", "from", "front", "frown", "fuck", "full", "fun", "funny", "further", "game", "gasp",
    "gave", "gaze", "gently", "get", "giggle", "girl", "girlfriend", "give", "given", "glad",
    "glance", "glare", "glass", "go", "God", "gone", "gonna", "good", "got", "gotten", "grab",
    "great", "green", "greet", "grey", "grin", "grip", "groan", "ground", "group", "grow", "guard",
    "guess", "gun", "guy", "had", "hadn't", "hair", "half", "hall", "hallway", "hand", "handle",
    "hang", "happen", "happy", "hard", "has", "hate", "have", "haven't", "he", "he'd", "he's",
    "head", "hear", "heard", "heart", "heavy", "held", "hell", "hello", "help", "her", "here",
    "herself", "hey", "hi", "hide", "high", "him", "himself", "his", "hit", "hold", "home", "hope",
    "horse", "hospital", "hot", "hour", "house", "how", "however", "hug", "huge", "huh", "human",
    "hundred", "hung", "hurry", "hurt", "I", "I'd", "I'll", "I'm", "I've", "ice", "idea", "if",
    "ignore", "imagine", "immediately", "important", "in", "inside", "instead", "interest",
    "interrupt", "into", "is", "isn't", "it", "it's", "its", "jacket", "jeans", "jerk", "job",
    "join", "joke", "jump", "just", "keep", "kept", "key", "kick", "kid", "kill", "kind", "kiss",
    "kitchen", "knee", "knew", "knock", "know", "known", "lady", "land", "large", "last", "late",
    "laugh", "lay", "lead", "lean", "learn", "least", "leave", "led", "left", "leg", "less", "let",
    "letter", "lie", "life", "lift", "light", "like", "line", "lip", "listen", "little", "live",
    "lock", "locker", "long", "look", "lose", "lost", "lot", "loud", "love", "low", "lunch", "mad",
    "made", "make", "man", "manage", "many", "mark", "marry", "match", "matter", "may", "maybe",
    "me", "mean", "meant", "meet", "memory", "men", "mention", "met", "middle", "might", "mind",
    "mine", "minute", "mirror", "miss", "mom", "moment", "money", "month", "mood", "more",
    "morning", "most", "mother", "mouth", "move", "movie", "Mr.", "Mrs.", "much", "mum", "mumble",
    "music", "must", "mutter", "my", "myself", "name", "near", "nearly", "neck", "need", "nervous",
    "never", "new", "next", "nice", "night", "no", "nod", "noise", "none", "normal", "nose", "not",
    "note", "nothing", "notice", "now", "number", "obviously", "of", "off", "offer", "office",
    "often", "oh", "okay", "old", "on", "once", "one", "only", "onto", "open", "or", "order",
    "other", "our", "out", "outside", "over", "own", "pack", "pain", "paint", "pair", "pants",
    "paper", "parents", "park", "part", "party", "pass", "past", "pause", "pay", "people",
    "perfect", "perhaps", "person", "phone", "pick", "picture", "piece", "pink", "piss", "place",
    "plan", "play", "please", "pocket", "point", "police", "pop", "position", "possible", "power",
    "practically", "present", "press", "pretend", "pretty", "probably", "problem", "promise",
    "pull", "punch", "push", "put", "question", "quick", "quickly", "quiet", "quietly", "quite",
    "race", "rain", "raise", "ran", "rang", "rather", "reach", "read", "ready", "real", "realize",
    "really", "reason", "recognize", "red", "relationship", "relax", "remain", "remember", "remind",
    "repeat", "reply", "respond", "rest", "return", "ride", "right", "ring", "road", "rock", "roll",
    "room", "rose", "round", "rub", "run", "rush", "sad", "safe", "said", "same", "sat", "save",
    "saw", "say", "scare", "school", "scream", "search", "seat", "second", "see", "seem", "seen",
    "self", "send", "sense", "sent", "serious", "seriously", "set", "settle", "seven", "several",
    "shadow", "shake", "share", "she", "she'd", "she's", "shift", "shirt", "shit", "shock", "shoe",
    "shook", "shop", "short", "shot", "should", "shoulder", "shouldn't", "shout", "shove", "show",
    "shower", "shrug", "shut", "sick", "side", "sigh", "sight", "sign", "silence", "silent",
    "simply", "since", "single", "sir", "sister", "sit", "situation", "six", "skin", "sky", "slam",
    "sleep", "slightly", "slip", "slow", "slowly", "small", "smell", "smile", "smirk", "smoke",
    "snap", "so", "soft", "softly", "some", "somehow", "someone", "something", "sometimes",
    "somewhere", "son", "song", "soon", "sorry", "sort", "sound", "space", "speak", "spend",
    "spent", "spoke", "spot", "stair", "stand", "star", "stare", "start", "state", "stay", "step",
    "stick", "still", "stomach", "stood", "stop", "store", "story", "straight", "strange", "street",
    "strong", "struggle", "stuck", "student", "study", "stuff", "stupid", "such", "suck", "sudden",
    "suddenly", "suggest", "summer", "sun", "suppose", "sure", "surprise", "surround", "sweet",
    "table", "take", "taken", "talk", "tall", "teacher", "team", "tear", "teeth", "tell", "ten",
    "than", "thank", "that", "that's", "the", "their", "them", "themselves", "then", "there",
    "there's", "these", "they", "they'd", "they're", "thick", "thing", "think", "third", "this",
    "those", "though", "thought", "three", "threw", "throat", "through", "throw", "tie", "tight",
    "time", "tiny", "tire", "to", "today", "together", "told", "tomorrow", "tone", "tongue",
    "tonight", "too", "took", "top", "totally", "touch", "toward", "town", "track", "trail",
    "train", "tree", "trip", "trouble", "trust", "truth", "try", "turn", "TV", "twenty", "two",
    "type", "uncle", "under", "understand", "until", "up", "upon", "us", "use", "usual", "usually",
    "very", "visit", "voice", "wait", "wake", "walk", "wall", "want", "warm", "warn", "was",
    "wasn't", "watch", "water", "wave", "way", "we", "we'll", "we're", "we've", "wear", "week",
    "weird", "well", "went", "were", "weren't", "wet", "what", "what's", "whatever", "when",
    "where", "whether", "which", "while", "whisper", "white", "who", "whole", "why", "wide", "wife",
    "will", "wind", "window", "wipe", "wish", "with", "within", "without", "woke", "woman", "women",
    "won't", "wonder", "wood", "word", "wore", "work", "world", "worry", "worse", "would",
    "wouldn't", "wow", "wrap", "write", "wrong", "yeah", "year", "yell", "yes", "yet", "you",
    "you'd", "you'll", "you're", "you've", "young", "your", "yourself"
  )
  // @formatter:on

}

package test {

  import money._, keyval._, model._
  import Currency.{ EUR, USD }

  import cats.implicits._

  import enumeratum._

  import refined.refineV
  import refined.api.Refined
  import refined.collection.NonEmpty
  import refined.numeric._

  import org.scalacheck._
  import org.scalacheck.ScalacheckShapeless._
  import Arbitrary.arbitrary

  object Jt8Gen {

    import time._

    def durationGen: Gen[Duration]                           = ???
    def finiteDurationGen(range: Duration): Gen[Duration]    = ???
    def localDateTimeInPeriod(p: Period): Gen[LocalDateTime] = ???

    lazy val start                    = java.time.Instant.EPOCH
    lazy val end                      = ((start atZone ZoneIdZ) + 100.years).toInstant
    lazy val oneHundredYearsOfSeconds = end.toEpochMilli / 1000

    implicit def arbitraryInstantE10: Arbitrary[Instant] =
      Arbitrary {
        for {
          secs <- Gen.chooseNum(0L, oneHundredYearsOfSeconds)
        } yield java.time.Instant.ofEpochSecond(secs)
      }
  }

  /** Nuts exist in the test package. Make of that what you will. */
  sealed trait Nut extends EnumEntry with Serializable

  /** Deez are maybe the nuts you are looking for. */
  object Nut extends DtEnum[Nut] {

    case object Peanut     extends Nut
    case object Hazelnut   extends Nut
    case object Almond     extends Nut
    case object Cashew     extends Nut
    case object Walnut     extends Nut
    case object Pecan      extends Nut
    case object Pistaschio extends Nut
    case object Brazil     extends Nut

    lazy val values = findValues
  }

  object currencies {

    implicit def arbitraryMny[C: Currency]: Arbitrary[Money[C]] =
      Arbitrary {
        import Financial.Ops
        val fiat = Currency[C]

        for {
          amount <- arbitrary[Double]
        } yield fiat(amount.to[model.MonetaryAmount])
      }

    type Dollars = Money[USD]
    lazy val Dollars                                   = USD
    def dollars(amount: model.MonetaryAmount): Dollars = Dollars(amount)

    type Euros = Money[EUR]
    lazy val Euros                                 = EUR
    def euros(amount: model.MonetaryAmount): Euros = Euros(amount)
  }

  object invoices {

    import time._, keyval._

    import currencies._

    sealed abstract case class Invoice(
        asOf: Instant,
        nut: Nut,
        quantity: Int Refined Positive,
        from: Party.Key,
        to: Party.Key,
        amount: Dollars,
        memo: String Refined NonEmpty
    )

    object Invoice extends WithOpaqueKey[Long, Invoice] {

      def mk(
          nut: Nut,
          jars: Int,
          from: Party.Key,
          to: Party.Key,
          total: Double,
          instructions: String = ""
      ): Invoice = {

        val Right(quantity) = refineV[Positive](jars min 1)
        val Right(memo)     = refineV[NonEmpty](s"special instructions: $instructions")
        val amount          = dollars(total)

        new Invoice(asOf = instant, nut, quantity, from, to, amount, memo) {}
      }
    }
  }
}
