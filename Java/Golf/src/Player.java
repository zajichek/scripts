import java.util.Vector;


public class Player {

	Card[] hand;
	boolean out;
	boolean[] flipped;
	int flipPosition;
	int points;
	
	
	public Player(Card[] c) {
		hand = new Card[6];
		hand = c;
		points = 0;
		flipped = new boolean[6];
		flipPosition = 0;
		flip();
		flip();
	}
	
	public void flip() {
		flipped[flipPosition] = true;
		flipPosition++;
	}
	
	public int getFlipPosition() {
		if(flipPosition < 6) {
			return flipPosition;
		} else {
			return -1;
		}
	}

	public Card swit(Card c) {
		Card topCard = hand[flipPosition];
		hand[flipPosition] = c;
		flip();
		return topCard;
	}

	public int getPoints() {
		int points = 0;
		Vector<Integer> v = new Vector<>();
		for(int i = 0; i < 6; i++) {
			if(flipped[i] == true) {
				v.addElement(i);
			}
		}
		
		while(v.size() > 1) {
		int j = 1;
		while(j < v.size()) {
			if(hand[v.elementAt(0)].number == hand[v.elementAt(j)].number) {
				v.removeElementAt(j);
				j = j - 1;
			}
			j++;
		}
			points = points + (cardCount(hand[v.elementAt(0)]) * hand[v.elementAt(0)].pointValue());
			v.removeElementAt(0);
		}
		
		if(v.size() > 0) { 
			points = points + hand[v.elementAt(0)].pointValue();
		}
		
		return points;
	}
	
	public int cardCount(Card c) {
		int count = 0;
		for(int i = 0; i < hand.length; i++) { 
			if(flipped[i] == true) {
				if(hand[i].number == c.number) {
					count = count + 1;
				}
			}
		}
			return count % 2;
	}
}
