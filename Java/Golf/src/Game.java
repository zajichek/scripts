import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class Game extends Component {
	Player[] players;
	Deck d;
	Card topCard;
	int turn;
	int[] x, y;
	JButton[] p1_buttons, p2_buttons;
	JButton newCard_button, skip_button;
	JLabel label, l1, l2, over_label;
	JFrame frame;
	int[] hole;
	int n;
	boolean z;
	
	public Game() {
		players = new Player[2];
		d = new Deck();
		d.shuffle();
		Card[] h1 = new Card[6];
		Card[] h2 = new Card[6];
		for(int i = 0; i < 6; i++) {
			h1[i] = d.draw();
			h2[i] = d.draw();
		}
		
		players[0] = new Player(h1);
		players[1] = new Player(h2);
		
		hole = new int[9];
		for(int i = 0; i < hole.length; i++) {
			hole[i] = i+1;
		}
		
		topCard = d.draw();
		
		//Start with player 1 (indicated by 0 in the array)
		turn = 0;
		
		x = new int[6];
		y = new int[6];
		
		p1_buttons = new JButton[6];
		p2_buttons = new JButton[6];
	
		newCard_button = new JButton();
		skip_button = new JButton("Skip");
		
		label = new JLabel();
		l1 = new JLabel();
		l2 = new JLabel();
		over_label = new JLabel();
		
		frame = new JFrame("GOLF");
		n = 0;
		z = false;
		initiateVisuals();
	}
	
	public void initiateVisuals() {

		frame.setSize(1500,1000);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		coord(10,100);
		for(int j = 0; j < 6; j++) {
			p1_buttons[j] = new JButton();
			p1_buttons[j].setBounds(x[j], y[j], 100, 150);
			p1_buttons[j].setOpaque(false);
			p1_buttons[j].setContentAreaFilled(false);
			p1_buttons[j].setBorderPainted(false);
			p1_buttons[j].addActionListener(new CardButtonAction(0, j));
		}
		
		coord(850, 100);
		for(int i = 0; i < 6; i++) {
			p2_buttons[i] = new JButton();
			p2_buttons[i].setBounds(x[i], y[i], 100, 150);
			p2_buttons[i].setOpaque(false);
			p2_buttons[i].setContentAreaFilled(false);
			p2_buttons[i].setBorderPainted(false);
			p2_buttons[i].addActionListener(new CardButtonAction(1, i));
			p2_buttons[i].setEnabled(false);
		}
		
		newCard_button.setBounds(610, 100, 100, 150);
		newCard_button.setOpaque(false);
		newCard_button.setContentAreaFilled(false);
		newCard_button.setBorderPainted(false);
		newCard_button.addActionListener(new NewCardAction());
		
		skip_button.setBounds(555, 250, 100,30);
		skip_button.addActionListener(new SkipTurnAction());

		label.setText("Player " + (turn + 1) + "'s turn!");
		label.setBounds(450, 300, 500, 100);
		label.setFont(new Font("Times New Roman",1,50));
		
		l1.setText("Player 1 points: " + players[0].getPoints());
		l1.setBounds(10, 400, 300, 100);
		l1.setFont(new Font("Times New Roman", 1, 30));

		l2.setText("Player 2 points: " + players[1].getPoints());
		l2.setBounds(850, 400, 300, 100);
		l2.setFont(new Font("Times New Roman", 1, 30));
		
		over_label.setText("GAME OVER!!!");
		over_label.setBounds(425,500, 1000, 100);
		over_label.setFont(new Font("Times New Roman", 1, 50));
		over_label.setVisible(false);
		
		for(int m = 0; m < 6; m++) {
			frame.add(p1_buttons[m]);
			frame.add(p2_buttons[m]);
		}
		
		frame.add(over_label);
		frame.add(newCard_button);
		frame.add(skip_button);
		frame.add(label);
		frame.add(l1);
		frame.add(l2);
		frame.add(this);
		frame.setVisible(true);
	}
	
	public void paint(Graphics g) {
		coord(10,100);
		for(int i = 0; i < players.length; i++) {
			for(int j = 0; j < 6; j++) {
				if(players[i].flipped[j] == true) {
					g.drawImage(players[i].hand[j].card, x[j], y[j], 100, 150, null);
				} else {
					g.setColor(new Color(5, 145, 35));
					g.fillRect(x[j], y[j], 100, 150);
				}
			}
			coord(850, 100);
		}
		
		g.drawImage(topCard.card, 500, 100, 100, 150, null);
		g.setColor(new Color(5, 145, 35));
		g.fillRect(610, 100, 100, 150);
	}
	
	//Returns the index of array of the player who's up next
	public int changeTurn() {
		if(turn == 0) {
			turn = 1;
		} else {
			turn = 0;
		}
		return turn;
	}
	
	public void coord(int x_init, int y_init) {
		x[0] = x_init; y[0] = y_init;
		x[1] = x[0] + 110; y[1] = y[0];
		x[2] = x[1] + 110; y[2] = y[1];
		x[3] = x[0]; y[3] = y_init + 160;
		x[4] = x[1]; y[4] = y[3];
		x[5] = x[2]; y[5] = y[3];
	}
	
	public boolean finalRound() {
		boolean lastTurn = false;
		int count = 0;
		for(int i = 0; i < 6; i++) {
			if(players[turn].flipped[i] == true) {
				count = count + 1;
			}
		}
		if(count == 6) {
			lastTurn = true;
		}
		return lastTurn;
	}
	
	public void disable() {
		if(turn == 0) {
		for(int i = 0; i < 6; i++) {
			p1_buttons[i].setEnabled(false);
			p2_buttons[i].setEnabled(true);
		}
		} else {
		for(int i = 0; i < 6; i++) {
			p2_buttons[i].setEnabled(false);
			p1_buttons[i].setEnabled(true);
		}
		}
	}
	
	public void flipAll() {
		for(int i = 0; i < 6; i++) {
			if(players[turn].flipped[i] == false) {
				players[turn].flipped[i] = true;
			}
		}
	}
	
	public boolean over() {
		boolean gameover = false;
		int count = 0;
		for(int i = 0; i < 6; i++) {
			if(players[0].flipped[i] == true && players[1].flipped[i] == true) {
				count = count+1;
			}
		}
		
		if(count == 6) {
			gameover = true;
		}
		
		return gameover;
		
	}
	
	
	//ActionListener classes for the buttons
	private class CardButtonAction implements ActionListener {
		
		int player, cardIndex;
		
		private CardButtonAction(int p, int c) {
			player = p;
			cardIndex = c;
		}
	
		public void actionPerformed(ActionEvent e) {
			Card temp = topCard;
			topCard = players[player].hand[cardIndex];
			players[player].hand[cardIndex] = temp;
			players[player].flipped[cardIndex] = true;
			
			if(player == 0) {
				l1.setText("Player 1 points: " + players[0].getPoints());
			} else {
				l2.setText("Player 2 points: " + players[1].getPoints());
			}
			
			newCard_button.setEnabled(true);
			disable();
			//changeTurn();
			//label.setText("Player " + (turn + 1) + "'s turn!");
			
			if(finalRound() && z == false) {
				changeTurn();
				label.setText("Final turn!!!");
				//flipAll();
				n = -3;
				z = true;
			} else if( n >= 0) {
				changeTurn();
				label.setText("Player " + (turn + 1) + "'s turn!");
			}
			n = n + 1;
			
			if(n == -1) {
				flipAll();
				if(player == 0) {
					l1.setText("Player 1 points: " + players[0].getPoints());
				} else {
					l2.setText("Player 2 points: " + players[1].getPoints());
				}
			}
			
			if(over()) {
				over_label.setVisible(true);
				label.setVisible(false);
			}
			
			frame.repaint();
		}
	}
	
	
	private class NewCardAction implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			topCard = d.draw();
			newCard_button.setEnabled(false);
			frame.repaint();
		}
	}
	
	
	private class SkipTurnAction implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			disable();
			//changeTurn();
			//label.setText("Player " + (turn + 1) + "'s turn!");
			newCard_button.setEnabled(true);
			
			if(finalRound() && z == false) {
				changeTurn();
				label.setText("Final turn!!!");
				//flipAll();
				n = -3;
				z = true;
			} else if( n >= 0) {
				changeTurn();
				label.setText("Player " + (turn + 1) + "'s turn!");
			}
			n = n + 1;
			
			if(n == -1) {
				flipAll();
				if(turn == 0) {
					l1.setText("Player 1 points: " + players[0].getPoints());
				} else {
					l2.setText("Player 2 points: " + players[1].getPoints());
				}
			}
			
			if(over()) {
				over_label.setVisible(true);
				label.setVisible(false);
			}
			
			frame.repaint();
		}
	}
}
