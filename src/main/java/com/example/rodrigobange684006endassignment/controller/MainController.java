package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.ItemDatabase;
import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import javafx.event.ActionEvent;
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
    @FXML
    VBox mainLayout;
    @FXML
    Button btnLendingReceiving;
    @FXML
    Button btnCollection;
    @FXML
    Button btnMembers;
    @FXML
    Button btnLogOut;
    @FXML
    Label lblWelcomeUsername;

    // Databases
    ItemDatabase itemDatabase;
    MemberDatabase memberDatabase;

    // Variables
    String lendingReceivingScene = "lending-receiving-view.fxml";
    String collectionScene = "collection-view.fxml";
    String membersScene = "members-view.fxml";

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle rb){
        // Initialize databases
        itemDatabase = new ItemDatabase();
        memberDatabase = new MemberDatabase();

        // Load default scene
        loadScene(lendingReceivingScene, new LendingReceivingController(itemDatabase, memberDatabase));
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
            throw new RuntimeException(e);
        }
    }

    public void displayUsername(String username) {
        /* Display welcome message
        Only gets called on scene opening */
        lblWelcomeUsername.setText("Welcome, " + username);
    }

    @FXML
    protected void onButtonLendingReceivingClick() {
        // Load Lending / Receiving scene
        loadScene(lendingReceivingScene, new LendingReceivingController(itemDatabase, memberDatabase));
    }

    @FXML
    protected void onButtonCollectionClick() {
        // Load Collection scene
        loadScene(collectionScene, new ItemCollectionController(itemDatabase));
    }

    @FXML
    protected void onButtonMembersClick() {
        // Load Member scene
        loadScene(membersScene, new MemberCollectionController(memberDatabase));
    }
}
