import java.util.Random;
import java.util.Scanner;
import java.io.IOException;


public class CardDeck {
    public static void main(String[] args) {
        clear();

        Scanner sc = new Scanner(System.in);
        Deck deck = new Deck();
        int option;
        
        do {
            menu();
            option = readInt(sc, "\n>> ");
            clear();
    
            switch(option) {
                case 0: break;
                case 1: deck.printFromTop(); break;
                case 2: deck.printFromBottom(); break;
                case 3: deck.sort(); break;
                case 4: deck.reverse(); break;
                case 5: deck.shuffle(); break;
                case 6: deck.inFaro(); break;
                case 7: deck.outFaro(); break;
                case 8: deck.mnemonica(); break;
                default: System.out.println("Invalid option!"); break;
            }
        } while(option != 0);
    
        sc.close();
    }

    public static void clear() {
        try {
            if (System.getProperty("os.name").contains("Windows"))
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            else
                Runtime.getRuntime().exec("clear");
        } catch (IOException | InterruptedException ex) {}
    }

    public static int readInt(Scanner sc, String message) {
        System.out.print(message);
        return sc.nextInt();
    }

    public static void menu() {
        System.out.println("\n== Menu ==\n");
        System.out.println("\t1 - print from top");
        System.out.println("\t2 - print from bottom");
        System.out.println("\t3 - sort");
        System.out.println("\t4 - reverse");
        System.out.println("\t5 - shuffle");
        System.out.println("\t6 - in faro");
        System.out.println("\t7 - out faro");
        System.out.println("\t8 - mnemonica");
        System.out.println("\n\t0 - exit");
    }
}


class Card {
    private final String value;
    private final String suit;

    protected Card(String value, String suit) {
        this.value = value;
        this.suit = suit;
    }

    protected int getValue() {
        int value = 0;

        switch(this.value) {
            case "A": value = 1; break;
            case "2": value = 2; break;
            case "3": value = 3; break;
            case "4": value = 4; break;
            case "5": value = 5; break;
            case "6": value = 6; break;
            case "7": value = 7; break;
            case "8": value = 8; break;
            case "9": value = 9; break;
            case "10": value = 10; break;
            case "Jack": value = 11; break;
            case "Queen": value = 12; break;
            case "King": value = 13; break;
            default: throw new java.lang.Error("<Error> getting card value");
        }

        return value;
    }

    protected int getSuit() {
        int suit = 0;
        
        switch(this.suit) {
            case "Spades": suit = 1; break;
            case "Diamonds": suit = 2; break;
            case "Clubs": suit = 3; break;
            case "Hearts": suit = 4; break;
            default: throw new java.lang.Error("<Error> getting card suit");
        }

        return suit;
    }

    public String toString() {
        String value = this.value != "10" ? " " + this.value.charAt(0) : this.value;
        String suit = this.suit.charAt(0) + "";
        
        return " " + value + suit + " - " + this.value + " of " + this.suit;
    }
}


class Deck {
    private Card[] deck = new Card[52];
    
    public Deck() {
        int vIndex = 0;
        int sIndex = 0;
        String[] Values = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"};
        String[] Suits = {"Spades", "Diamonds", "Clubs", "Hearts"};

        for(int i = 0; i < this.deck.length / 2; i++) {
            this.deck[i] = new Card(Values[vIndex], Suits[sIndex]);
            this.deck[this.deck.length - i - 1] = new Card(Values[vIndex], Suits[Suits.length - sIndex - 1]);
            
            vIndex++;
            
            if(vIndex >= 13) {
                vIndex = 0;
                sIndex++;
            }
        }
    }

    public void printFromBottom() {
        System.out.println();

        for(int i = 0; i < this.deck.length; i++) {
            System.out.println(this.deck[i]);
            
            if((i + 1) % 13 == 0)
                System.out.println();
        }

        System.out.println();
    }
    
    public void printFromTop() {
        System.out.println();

        for(int i = this.deck.length - 1; i >= 0; i--) {
            System.out.println(this.deck[i]);
            
            if(i % 13 == 0)
                System.out.println();
        }

        System.out.println();
    }
    
    public void sort() {
        final int length = this.deck.length;
        
        for(int i = 0; i < length; i++) {
            for(int j = 0; j < length - 1; j++) {
                if(this.deck[j].getSuit() > this.deck[j + 1].getSuit()) {
                    Card tmp = this.deck[j];
                    this.deck[j] = this.deck[j + 1];
                    this.deck[j + 1] = tmp;
                }
            }
        }
        
        for(int i = 0; i < length / 2; i++) {
            for(int j = 0; j < (length / 2) - 1; j++) {
                if(this.deck[j].getValue() > this.deck[j + 1].getValue() && j != (length / 4) - 1) {
                    Card tmp = this.deck[j];
                    this.deck[j] = this.deck[j + 1];
                    this.deck[j + 1] = tmp;
                }
    
                if(this.deck[j + (length / 2)].getValue() < this.deck[j + (length / 2) + 1].getValue() && j != (length / 4) - 1) {
                    Card tmp = this.deck[j + (length / 2)];
                    this.deck[j + (length / 2)] = this.deck[j + (length / 2) + 1];
                    this.deck[j + (length / 2) + 1] = tmp;
                }
            }
        }
    }

    public void reverse() {
        for(int i = 0; i <= this.deck.length / 2; i++) {
            Card tmp = this.deck[i];
            this.deck[i] = this.deck[this.deck.length - i - 1];
            this.deck[this.deck.length - i - 1] = tmp;
        }
    }
    
    public void shuffle() {
        int card1, card2;
        
        for(int i = 0; i < this.deck.length; i++) {
            card1 = new Random().nextInt(this.deck.length);
            do
                card2 = new Random().nextInt(this.deck.length);
            while(card1 == card2);

            Card tmp = this.deck[card1];
            this.deck[card1] = this.deck[card2];
            this.deck[card2] = tmp;
        }
    }
    
    public void inFaro() {
        final int length = this.deck.length;
        Card[] tmpDeck = new Card[length];
        
        for(int i = 0; i < length / 2; i++) {
            tmpDeck[(i * 2) + 1] = this.deck[i];
            tmpDeck[i * 2] = this.deck[(length / 2) + i];
        }

        this.deck = tmpDeck;
    }

    public void outFaro() {
        final int length = this.deck.length;
        Card[] tmpDeck = new Card[length];

        for(int i = 0; i < length / 2; i++) {
            tmpDeck[i * 2] = this.deck[i];
            tmpDeck[((i + (length / 2)) * 2) - length + 1] = this.deck[i + (length / 2)];
        }
        
        this.deck = tmpDeck;
    }
    
    public void mnemonica() {
        this.sort();
        
        final int length = this.deck.length;
        Card[] tmpDeck = new Card[length];

        for(int i = 0; i < length / 4; i++) {
            tmpDeck[i] = this.deck[(length - (length / 4)) - (i + 1)];
            tmpDeck[(length / 4) + i] = this.deck[(length / 4) + i];
            tmpDeck[(length / 2) + i] = this.deck[((length / 4) * 3) + i];
            tmpDeck[((length / 4) * 3) + i] = this.deck[(length / 4) - (i + 1)];
        }

        this.deck = tmpDeck;

        this.outFaro();
        this.outFaro();
        this.outFaro();
        this.outFaro();

        for(int i = 0; i < length / 4; i++) {
            Card tmp = this.deck[(length / 2) + i];
            this.deck[(length / 2) + i] = this.deck[length - (i + 1)];
            this.deck[length - (i + 1)] = tmp;
        }
        
        Card[] tmpABitMoreThanAQuarterDeck1 = new Card[18];
        Card[] tmpABitMoreThanAQuarterDeck2 = new Card[18];
        
        for(int i = 0; i < 18; i++) {
            tmpABitMoreThanAQuarterDeck1[i] = this.deck[i + length - 36];
            tmpABitMoreThanAQuarterDeck2[i] = this.deck[i + length - 18];
        }
        
        for(int i = 0; i < 18; i++) {
            this.deck[length - 36 + (2 * i)] = tmpABitMoreThanAQuarterDeck1[i];
            this.deck[length - 36 + (2 * i) + 1] = tmpABitMoreThanAQuarterDeck2[i];
        }

        for(int i = 0; i < 9; i++)
            tmpDeck[i] = this.deck[length - 9 + i];

        for(int i = 9; i < length; i++)
            tmpDeck[i] = this.deck[i - 9];

        this.deck = tmpDeck;
    }
}
