import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.io.IOException;

public class SnakeGame{
    public static void clear(){
        try{
            if(System.getProperty("os.name").contains("Windows"))
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            else
                Runtime.getRuntime().exec("clear");
        }catch(IOException | InterruptedException ex){}
    }

    public static void main(String[] args) throws InterruptedException{
        clear();

        Scanner sc = new Scanner(System.in);

        Game game = new Game();
        game.menu(sc);

        sc.close();
    }
}

class Nodes{
    protected int x;
    protected int y;
    protected char icon;

    Nodes(int x, int y, char icon){
        this.x = x;
        this.y = y;
        this.icon = icon;
    }
}

class Snake{
    protected Nodes[] node;
    protected int length;
    protected boolean alive;
    protected String direction;

    Snake(){
        this.node = new Nodes[100];
        this.length = 5;
        this.alive = true;
        this.direction = "east";

        this.node[0] = new Nodes(4, 14, '>');
        this.node[1] = new Nodes(5, 14, '>');
        this.node[2] = new Nodes(6, 14, '>');
        this.node[3] = new Nodes(7, 14, '>');
        this.node[4] = new Nodes(8, 14, '>');
    }

    protected void append(){
        int tmpX = this.node[this.length - 1].x;
        int tmpY = this.node[this.length - 1].y;
        char tmpIcon = '>';

        switch(this.direction){
            case "north": tmpY--; tmpIcon = '^'; break;
            case "east": tmpX++; tmpIcon = '>'; break;
            case "south": tmpY++; tmpIcon = 'v'; break;
            case "west": tmpX--; tmpIcon = '<'; break;
        }
        
        this.node[this.length] = new Nodes(tmpX, tmpY, tmpIcon);
        this.length++;
    }
}

class Game{
    private String[] map;
    private int points;
    private Nodes dot;
    private Snake snake;
    private BlockingQueue<String> lines = new LinkedBlockingQueue<>();

    public Game(){
        this.map = new String[21];
        this.points = 0;
        this.snake = new Snake();
        this.dot = new Nodes(30, 4, '*');
        
        this.updateMap();
    }

    private void printMap(){
        SnakeGame.clear();

        for(int i = 0; i <= 20; i++)
            System.out.println(map[i]);
    }

    private void updateMap(){
        this.map[0]  = " ___________________________________________________";
        this.map[1]  = "|                                                   |";
        this.map[2]  = "|                                                   |";
        this.map[3]  = "|                                                   |";
        this.map[4]  = "|                                                   |";
        this.map[5]  = "|                                                   |";
        this.map[6]  = "|                                                   |";
        this.map[7]  = "|                                                   |";
        this.map[8]  = "|                                                   |";
        this.map[9]  = "|                                                   |";
        this.map[10] = "|                                                   |";
        this.map[11] = "|                                                   |";
        this.map[12] = "|                                                   |";
        this.map[13] = "|                                                   |";
        this.map[14] = "|                                                   |";
        this.map[15] = "|                                                   |";
        this.map[16] = "|                                                   |";
        this.map[17] = "|                                                   |";
        this.map[18] = "|                                                   |";
        this.map[19] = "|                                                   |";
        this.map[20] = "|___________________________________________________|";

        if(snake.node[snake.length - 1].x == dot.x && snake.node[snake.length - 1].y == dot.y){
            this.points++;

            snake.append();
            updateDot();
        }
        
        for(int i = 0; i < snake.length; i++){            
            if(map[snake.node[i].y].charAt(snake.node[i].x) == ' ')
                map[snake.node[i].y] = map[snake.node[i].y].substring(0, snake.node[i].x) + snake.node[i].icon + map[snake.node[i].y].substring(snake.node[i].x + 1, map[snake.node[i].y].length());

            if(map[dot.y].charAt(dot.x) == ' ')
                map[dot.y] = map[dot.y].substring(0, dot.x) + '*' + map[dot.y].substring(dot.x + 1, map[dot.y].length());
        }
    }

    private void turnTo(String direction){
        if(direction.equals("east") && snake.direction != "west")
            this.snake.direction = "east";
        else if(direction.equals("north") && snake.direction != "south")
            this.snake.direction = "north";
        else if(direction.equals("south") && snake.direction != "north")
            this.snake.direction = "south";
        else if(direction.equals("west") && snake.direction != "east")
            this.snake.direction = "west";
    }

    private void move(){
        for(int i = 0; i < snake.length - 1; i++){
            snake.node[i].x = snake.node[i + 1].x;
            snake.node[i].y = snake.node[i + 1].y;
            snake.node[i].icon = snake.node[i + 1].icon;
        }

        if(snake.node[snake.length - 1].x == 0 && snake.direction == "west")
            snake.node[snake.length - 1].x = 51;
        else if(snake.node[snake.length - 1].x == 51 && snake.direction == "east")
            snake.node[snake.length - 1].x = 0;
        else if(snake.node[snake.length - 1].y == 1 && snake.direction == "north")
            snake.node[snake.length - 1].y = 19;
        else if(snake.node[snake.length - 1].y == 19 && snake.direction == "south")
            snake.node[snake.length - 1].y = 1;
        else
            switch(snake.direction){
                case "east": snake.node[snake.length - 1].x += 1; snake.node[snake.length - 1].icon = '>'; break;
                case "south": snake.node[snake.length - 1].y += 1; snake.node[snake.length - 1].icon = 'v'; break;
                case "west": snake.node[snake.length - 1].x -= 1; snake.node[snake.length - 1].icon = '<'; break;
                case "north": snake.node[snake.length - 1].y -= 1; snake.node[snake.length - 1].icon = '^'; break;
            }

        for(int i = 0; i < snake.length - 1; i ++){
            if(snake.node[snake.length - 1].x == snake.node[i].x && snake.node[snake.length - 1].y == snake.node[i].y){
                this.gameOver();
                return;
            }
        }

        this.updateMap();
        this.printMap();
    }

    private void updateDot(){
        Random rand = new Random();

        int randX = rand.nextInt(2, 50);
        int randY = rand.nextInt(2, 18);

        for(int i = 0; i < snake.length; i++){
            if(snake.node[i].x == randX && snake.node[i].y == randY){
                i = 0;

                randX = rand.nextInt(2, 50);
                randY = rand.nextInt(2, 18);
            }
        }

        dot.x = randX;
        dot.y = randY;
    }

    private void gameOver(){
        SnakeGame.clear();

        snake.alive = false;

        System.out.println("\n\n\n\n\n\n\n\n\n                     Game Over");
        System.out.println("\n                     Points: " + points + "\n\n\n\n\n\n\n\n\n");
    }
    
    private void run(Scanner sc) throws InterruptedException{
        Thread t = new Thread(() -> {
            while(snake.alive){
                lines.add(sc.nextLine());
            }
        });
        
        t.setDaemon(true);
        t.start();
        
        while(snake.alive){
            String input = this.lines.poll(400, TimeUnit.MILLISECONDS);
            
            if(input != null){
                switch(input){
                    case "w": this.turnTo("north"); break;
                    case "a": this.turnTo("west"); break;
                    case "s": this.turnTo("south"); break;
                    case "d": this.turnTo("east"); break;
                }
            }

            this.move();
        }
    }
    
    public void menu(Scanner sc) throws InterruptedException{
        String input;

        do{
            SnakeGame.clear();

            System.out.println("\n\n                     Snake Game\n\n");
            System.out.println("\tTutorial:\n");
            System.out.println("\tW + ENTER - move up");
            System.out.println("\tA + ENTER - move left");
            System.out.println("\tS + ENTER - move down");
            System.out.println("\tD + ENTER - move right");
            System.out.println("\n\t0 - Start");
            System.out.print("\n\n\t>> ");
        
            input = sc.nextLine();
        }while(!input.equals("0"));

        this.run(sc);
    }
}
