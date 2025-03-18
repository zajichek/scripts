import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;


public class Card {
	String suit;
	int number;
	BufferedImage card;
	String filename;

	public Card(int number, String suit) {
		this.suit = suit;
		this.number = number;
		
		filename = new String();
		if(number == 0) {
			filename = "red_joker.png";
		} else if(number == 11) {
			filename = "jack_of_" + suit + ".png";
		} else if(number == 12) {
			filename = "queen_of_" + suit + ".png";
		} else if(number == 13) {
			filename = "king_of_" + suit + ".png";
		} else if(number == 14) {
			filename = "ace_of_" + suit + ".png";
		} else {
			filename = number + "_of_" + suit + ".png";
		}
		
		card = null;
		
		  try {
			card = ImageIO.read(new File(filename));
		} catch(IOException e) {
			e.getMessage();
		}
		
	}
		

	public String getSuit() {
		return suit;
	}
	
	public String getNumber() {
		String value;
		if(number == 0) {
			value = "";
		} else if(number == 11) {
			value = "Jack";
		} else if(number == 12) {
			value = "Queen";
		} else if(number == 13) {
			value = "King";
		} else if(number == 14) {
			value = "Ace";
		} else {
			value = Integer.toString(number);
		}
		return value;
	}
	
	public int pointValue() {
		int pv = number;
		if(number == 0) {
			pv = -2;
		} else if(number == 11 || number == 12) {
			pv = 10;
		} else if(number == 13) {
			pv = 0;
		} else if(number == 14) {
			pv = 1;
		}
		return pv;
	}
	
	public Image getImage() {
		return card;
	}
	
	public String getFilename() {
		return filename;
	}
}
