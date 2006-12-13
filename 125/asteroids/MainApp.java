package asteroids;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class MainApp implements ActionListener, WindowListener {
   
   private final static Dimension SMALL = new Dimension(320, 240);
   private final static Dimension MEDIUM = new Dimension(640, 480);
   private final static Dimension LARGE = new Dimension(800, 600);
   
   private static DisplayMode small_DM;
   private static DisplayMode med_DM;
   private static DisplayMode large_DM;
  
   private static DisplayMode originalMode;
   
   private JFrame frame;
   private JFrame fullFrame;
   private Asteroids game;
   
   private JMenuBar menuBar;
   private JMenu gameMnu;
   private JMenuItem newGameMI;
   private JMenuItem exitMI;
   
   private JMenu viewMenu;
   private JCheckBoxMenuItem useColorMI;
   private JMenuItem smallMI;
   private JMenuItem mediumMI;
   private JMenuItem largeMI;
   private JMenuItem fullScreenMI;
   private JButton exitFullScreenBtn;
   private JPanel fullScreenPanel;
   
   private JMenu helpMnu;
   private JMenuItem aboutMI;
   
   private boolean isFullScreen = false;
   private Dimension currentSize = MEDIUM;
   
   public MainApp() {
      
      init();
   }
   
   private void init() {
      
      game = new Asteroids();
      
      //Exit FullWindow
      exitFullScreenBtn = new JButton("Exit FullScreen");
      exitFullScreenBtn.addActionListener(this);
      fullScreenPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
      fullScreenPanel.add(exitFullScreenBtn);
      
      //Icon
	  initMenus();
      
      //Frame
      setupNormalWindowFrame();
      frame.setSize(currentSize);
      frame.setVisible(true);
      frame.validate();
                        
      game.init();
      game.start();
      game.setGameSize(currentSize);
   }
   
   private void initMenus() {
      
      menuBar = new JMenuBar();
      
      //Game Menu
      gameMnu = new JMenu("Game");
      gameMnu.setMnemonic('G');
      //gameMnu.getPopupMenu().setLightWeightPopupEnabled(false);
      
      //New Game
      newGameMI = new JMenuItem("New Game");
      newGameMI.setMnemonic('N');
      newGameMI.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_MASK));
      newGameMI.addActionListener(this);
      gameMnu.add(newGameMI);
      gameMnu.add(new JSeparator());
      
      //Exit
      exitMI = new JMenuItem("Exit");
      exitMI.setMnemonic('x');
      exitMI.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.CTRL_MASK));
      exitMI.addActionListener(this);
      menuBar.add(gameMnu);
      gameMnu.add(exitMI);
      
      //View Menu
      viewMenu = new JMenu("View");
      viewMenu.setMnemonic('V');
      //viewMnu.getPopupMenu().setLightWeightPopupEnabled(false);
      menuBar.add(viewMenu);
      
      //Use Color
      useColorMI = new JCheckBoxMenuItem("Use Color");
      useColorMI.setState(game.getUseColor());
      useColorMI.addActionListener(this);
      viewMenu.add(useColorMI);
      viewMenu.add(new JSeparator());
      
      //Small
      smallMI = new JMenuItem("Small");
      smallMI.setMnemonic('S');
      smallMI.addActionListener(this);
      viewMenu.add(smallMI);
      
      //Medium
      mediumMI = new JMenuItem("Medium");
      mediumMI.setMnemonic('M');
      mediumMI.addActionListener(this);
      viewMenu.add(mediumMI);
      
      //Large
      largeMI = new JMenuItem("Large");
      largeMI.setMnemonic('L');
      largeMI.addActionListener(this);
      viewMenu.add(largeMI);
      viewMenu.add(new JSeparator());
      
      //FullScreen
      fullScreenMI = new JMenuItem("Full Screen");
      fullScreenMI.setMnemonic('F');
      fullScreenMI.addActionListener(this);
      //fullScreenMI.setEnabled(false);
      viewMenu.add(fullScreenMI);
      
      //Help
      helpMnu = new JMenu("Help");
      helpMnu.setMnemonic('H');
      menuBar.add(helpMnu);
      
      aboutMI = new JMenuItem("About");
      aboutMI.setMnemonic('A');
      aboutMI.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
      aboutMI.addActionListener(this);
      helpMnu.add(aboutMI);
   }
   
   public void setFullScreenMode(boolean fullScreen) {
   
      try {
      if(isFullScreen == fullScreen) return;
      
      GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
      GraphicsDevice device = env.getDefaultScreenDevice();
      
      
      if (fullScreen && device.isFullScreenSupported()) {
         // Full-screen mode
         frame.setVisible(false);
         setupFullWindowFrame();
         device.setFullScreenWindow(fullFrame);
         if(currentSize.equals(SMALL)) {
            device.setDisplayMode(small_DM);
         }
         if(currentSize.equals(MEDIUM)) {
            device.setDisplayMode(med_DM);
         }
         if(currentSize.equals(LARGE)) {
            device.setDisplayMode(large_DM);
         }
      }
      else {
         //Normal Mode
         device.setDisplayMode(originalMode);
         fullFrame.setVisible(false);
         device.setFullScreenWindow(null);
         
         setupNormalWindowFrame();
         frame.setVisible(true);
         frame.pack();
      }
      
      isFullScreen = fullScreen;
      game.requestFocus();
      }
      catch(RuntimeException exp) {
         //Catch any problem setting fullscreen mode
         fullFrame.setVisible(false);
         setupNormalWindowFrame();
         frame.setVisible(true);
         frame.pack();
         JOptionPane.showMessageDialog(frame, 
            "The application may not have permission for full screen mode", 
            "FullScreen Error", JOptionPane.ERROR_MESSAGE);
      }
   }
   
   public void setupFullWindowFrame() {
      
      if(fullFrame == null) {
         fullFrame = new JFrame();
         fullFrame.setUndecorated(true);
         fullFrame.addWindowListener(this);
         fullFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
         fullFrame.getContentPane().add(fullScreenPanel, BorderLayout.NORTH);
      }
            
      fullFrame.getContentPane().add(game);
      fullFrame.pack();
   }
   
   public void setupNormalWindowFrame() {
      
      if(frame == null) {
         frame = new JFrame("Asteroids");
         frame.addWindowListener(this);
         frame.setResizable(false);
         frame.setJMenuBar(menuBar);
         frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
      }
      
      
      frame.getContentPane().add(game);
   }
   
   /**
    * Invoked when an action occurs.
    *
    */
   public void actionPerformed(ActionEvent e) {
      
      
      //New Game
      if(e.getSource() == newGameMI) {
         String msg = "Are you sure you want to start a new Game?";
         String title = "Asteroids";
         if(game.isPlaying()) {
            game.pause();
            int request = JOptionPane.showConfirmDialog(null, msg, title, JOptionPane.YES_NO_OPTION);
            game.pause();
            if(request == JOptionPane.YES_OPTION) game.init();
         }
      }
      
      //Exit
      else if(e.getSource() == exitMI) {
         int request = JOptionPane.YES_OPTION;
         if(game.isPlaying()) {
            game.pause();
            request = JOptionPane.showConfirmDialog(null, "Are you sure you want to Exit?", "Asteroids", JOptionPane.YES_NO_OPTION);
            game.pause();
         }
         
         if(request == JOptionPane.YES_OPTION) {
            game.stop();
            SwingUtilities.getWindowAncestor(game).dispose();
            System.exit(0);
         }
      }
      
      //Use Color
      else if(e.getSource() == useColorMI) {
         if(useColorMI.getState() != game.getUseColor()) game.toggleUseColor();
      }
      
      //FullScreen
      else if(e.getSource() == fullScreenMI) {
         setFullScreenMode(true);
      }
      else if(e.getSource() == exitFullScreenBtn) {
         setFullScreenMode(false);
      }
      else if(e.getSource() == smallMI) {
         currentSize = SMALL;
         game.setGameSize(SMALL);
         if(!isFullScreen) SwingUtilities.getWindowAncestor(game).pack();
      }
      else if(e.getSource() == mediumMI) {
         currentSize = MEDIUM;
         game.setGameSize(MEDIUM);
         if(!isFullScreen) SwingUtilities.getWindowAncestor(game).pack();
      }
      else if(e.getSource() == largeMI) {
         currentSize = LARGE;
         game.setGameSize(LARGE);
         if(!isFullScreen) SwingUtilities.getWindowAncestor(game).pack();
      }
      
      //Help
      else if(e.getSource() == aboutMI) {
         showHelp();
      }
   }
   
   public void showHelp() {
      
      if(isFullScreen) setFullScreenMode(false);
      
      final JFrame help = new JFrame("Asteroids Help");
      help.setResizable(false);
      ImageIcon imageIcon = new ImageIcon(getClass().getClassLoader().getResource("org/xito/asteroids/help.png"));
      help.getContentPane().add(new JLabel(imageIcon), BorderLayout.CENTER);
      
      JPanel buttonP = new JPanel(new FlowLayout(FlowLayout.RIGHT));
      
      JButton sourceBtn = new JButton("Get Source");
      sourceBtn.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent evt) {
            getSource();
         }
      });
      buttonP.add(sourceBtn);
      
      JButton closeBtn = new JButton("Close");
      closeBtn.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent evt) {
            help.dispose();
         }
      });
      buttonP.add(closeBtn);
      help.getContentPane().add(buttonP, BorderLayout.SOUTH);
      
      help.pack();
      help.setLocation(frame.getX()+((frame.getWidth()/2) - (help.getWidth()/2)), frame.getY());
      
      help.setVisible(true);
   }
   
   public void getSource() {
      //javax.jnlp.ServiceManager.lookup(BasicService.class)
   }
   
   public static void main(String args[]) {
      
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      }
      catch(Exception ex) {
         ex.printStackTrace();
      }
            
      GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
      GraphicsDevice device = env.getDefaultScreenDevice();
      
      originalMode = device.getDisplayMode();
      DisplayMode modes[] = device.getDisplayModes();
      for(int i=0;i<modes.length;i++) {
         DisplayMode mode = modes[i];
         //Small
         if(mode.getWidth() == SMALL.width && mode.getHeight() == SMALL.height) {
            if(small_DM == null) small_DM = mode;
            if(small_DM.getBitDepth() < mode.getBitDepth()) small_DM = mode;
         }
         //Med
         if(mode.getWidth() == MEDIUM.width && mode.getHeight() == MEDIUM.height) {
            if(med_DM == null) med_DM = mode;
            if(med_DM.getBitDepth() < mode.getBitDepth()) med_DM = mode;
         }
         //Large
         if(mode.getWidth() == LARGE.width && mode.getHeight() == LARGE.height) {
            if(large_DM == null) large_DM = mode;
            if(large_DM.getBitDepth() < mode.getBitDepth()) large_DM = mode;
         }
      }
      
      MainApp me = new MainApp();
      me.game.requestFocus();
   }
   
   public void windowActivated(WindowEvent e) {
   }
   
   public void windowClosed(WindowEvent e) {
      System.exit(0);
   }
   
   public void windowClosing(WindowEvent e) {
   }
   
   public void windowDeactivated(WindowEvent e) {
   }
   
   public void windowDeiconified(WindowEvent e) {
   }
   
   public void windowIconified(WindowEvent e) {
   }
   
   public void windowOpened(WindowEvent e) {
   }
   
}


