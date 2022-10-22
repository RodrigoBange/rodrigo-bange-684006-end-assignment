package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class MainController implements Initializable {
    Stage stage;
    @FXML
    VBox mainLayout;
    @FXML
    Label lblWelcomeUsername;

    // Database
    Database database;

    // Variables
    String lendingReceivingScene = "lending-receiving-view.fxml";
    String collectionScene = "collection-view.fxml";
    String membersScene = "members-view.fxml";
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
        } catch (IOException e) {
            // TODO: Display an error message somewhere
            new ErrorLogger().log(e);
        }
    }

    @FXML
    protected void onButtonLendingReceivingClick() {
        // Load Lending / Receiving scene
        loadScene(lendingReceivingScene, new LendingReceivingController(database));
    }

    @FXML
    protected void onButtonCollectionClick() {
        // Load Collection scene
        loadScene(collectionScene, new ItemCollectionController(database));
    }

    @FXML
    protected void onButtonMembersClick() {
        // Load Member scene
        loadScene(membersScene, new MemberCollectionController(database));
    }
}
