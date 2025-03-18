import java.util.Vector;

public class Deck {
	Card[] cards;
	String[] suits;
	int[] numbers;
	int pos;
	
	public Deck() {
		cards = new Card[54];
		suits = new String[4];
		numbers = new int[13];
		
		suits[0] = "hearts";
		suits[1] = "diamonds";
		suits[2] = "clubs";
		suits[3] = "spades";
		
		for(int i = 0; i < numbers.length; i++) {
			numbers[i] = i + 2;
		}
		
		int x = 0;
		for(int i = 0; i < suits.length; i++) {
			for(int j = 0; j < numbers.length; j++) {
				cards[x] = new Card(numbers[j], suits[i]);
				x++;
			}
		}
		
		cards[52] = new Card(0, "Joker");
		cards[53] = new Card(0, "Joker");
		
		pos = 0;
	}
	
	public Deck(Card[] c) {
		cards = new Card[c.length];
		suits = new String[4];
		numbers = new int[13];
		
		suits[0] = "Hearts";
		suits[1] = "Diamonds";
		suits[2] = "Clubs";
		suits[3] = "Spades";
		
		for(int i = 0; i < numbers.length; i++) {
			numbers[i] = i + 2;
		}
		
		cards = c;
		pos = 0;
	}
	
	public Card randomCard() {
		String suit = suits[((int) (Math.random()*101))%suits.length];
		int number = numbers[((int) (Math.random()*101))%numbers.length];
		Card temp = new Card(number, suit);
		return temp;
	}
	
	public void shuffle() {

		Card[] temp = new Card[cards.length];
		Vector<Integer> v = new Vector<>();
		for(int i = 0; i < cards.length; i++) {
			v.addElement(i);
		}
		
		int index = 0;
		int p = 0;
		while(p < 54) {
			index = ((int) (Math.random()*101))%v.size();
			temp[p] = cards[v.elementAt(index)];
			p++;
			v.removeElementAt(index);
		}
		cards = temp;
	}
	
	//Requested position starts from 1
	public Card cardAt(int p) {
		return cards[p - 1];
	}
	
	public Card draw() {
		Card c = cards[pos];
		pos = pos + 1;
		return c;
	}
	
	public int remainingCards() {
		return cards.length - pos;
	}
}
