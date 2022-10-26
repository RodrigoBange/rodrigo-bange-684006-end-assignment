package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class MainController implements Initializable {
    Stage stage;
    @FXML VBox mainLayout;
    @FXML Label lblWelcomeUsername;
    @FXML Label lblWarning;
    @FXML Button btnLendingReceiving;
    @FXML Button btnCollection;
    @FXML Button btnMembers;

    // Database
    Database database;

    // Variables
    String lendingReceivingScene = "lending-receiving-view.fxml";
    String collectionScene = "collection-view.fxml";
    String membersScene = "members-view.fxml";
    String loginScene = "login-view.fxml";
    String displayName;

    // Constructor
    public MainController(Database database, Stage stage, String displayName) {
        this.database = database;
        this.stage = stage;
        this.displayName = displayName;
    }

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle rb){
        // Load default scene
        loadScene(lendingReceivingScene, new LendingReceivingController(database));
        lblWelcomeUsername.setText("Welcome, " + displayName);

        // Set on close event
        stage.setOnCloseRequest(event -> saveCollections());
    }

    void saveCollections() {
        database.saveToFiles();
    }

    public void loadScene(String name, Object controller) {
        try {
            FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(name));
            fxmlLoader.setController(controller);
            Scene scene = new Scene(fxmlLoader.load());
            if (mainLayout.getChildren().size() > 1) {
                mainLayout.getChildren().remove(1);
            }

            mainLayout.getChildren().add(scene.getRoot());
            lblWarning.setText("");
        } catch (Exception e) {
            new ErrorLogger().log(e);
            lblWarning.setText("Could not retrieve the requested window. Please try again.");
        }
    }

    @FXML
    protected void onButtonLendingReceivingClick() {
        // Load Lending / Receiving scene
        loadScene(lendingReceivingScene, new LendingReceivingController(database));

        // Set button style
        setButtonStyling(btnLendingReceiving);
    }

    @FXML
    protected void onButtonCollectionClick() {
        // Load Collection scene
        loadScene(collectionScene, new ItemCollectionController(database));

        // Set button styling
        setButtonStyling(btnCollection);
    }

    @FXML
    protected void onButtonMembersClick() {
        // Load Member scene
        loadScene(membersScene, new MemberCollectionController(database));

        // Set button styling
        setButtonStyling(btnMembers);
    }

    /**
     * Sets the menu button styles.
     * @param button The button to adjust.
     */
    void setButtonStyling(Button button) {
        String standardStyle = "-fx-background-color: #1D1F2D; -fx-font-weight: normal; -fx-font-size: 16;";
        // Set all other buttons to default
        btnLendingReceiving.setStyle(standardStyle);
        btnCollection.setStyle(standardStyle);
        btnMembers.setStyle(standardStyle);

        // Activate button styling
        button.setStyle("-fx-background-color: #24283B; -fx-font-weight: bold;");
    }

    @FXML
    protected void onLogOutClick() {
        try {
            logOut();
        }
        catch (Exception e) {
            new ErrorLogger().log(e);
            lblWarning.setText("Could not retrieve the login window. Please try again.");
        }
    }

    /**
     * Attempts to log out the user
     */
    void logOut() throws IOException {
        // Save collections to database*
        // *OnCloseRequest() is only called by external requests to close the window.
        // **Calling the stage.firevent window close request will also not execute OnCloseRequest.
        saveCollections();

        // Initialize stage
        Stage newStage = new Stage();

        // Initialize FXMLLoader
        FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(loginScene));

        // Initialize scene and display login window
        Scene scene = new Scene(fxmlLoader.load(), 750, 500);
        newStage.setScene(scene);
        newStage.setTitle("Library System - Log in");
        newStage.sizeToScene();
        newStage.show();
        newStage.setResizable(false);

        // Close current window
        stage.close();
    }
}
