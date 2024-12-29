package com.example.vttp5a_miniproject.service;

import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.example.vttp5a_miniproject.model.User;
import com.example.vttp5a_miniproject.repo.RedisRepo;

@Service
public class UserService {
    @Autowired
    private RedisRepo repo;

    public void registerUser(String username, String password, String email) {
        if (repo.userExists(username)) {
            throw new IllegalStateException("username already taken");
        } // check if username exists

        // create user
        Map<String, String> userData = new HashMap<>();
        userData.put("password", password);
        userData.put("email", email);
        repo.saveUser(username, userData);
    }

    public boolean loginUser(String username, String password) {
        if (!repo.userExists(username)) {
            throw new IllegalArgumentException("User does not exist. Please register first.");
        }
            Map<Object, Object> userData = repo.getUser(username);
        if (userData == null || userData.isEmpty()) {
            throw new IllegalArgumentException("User data is missing");
        }
            String storedPassword = (String) userData.get("password");
        return password.equals(storedPassword);
    }
    

    // get user profile
    public User getUserProfile(String username) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be empty");
        }

        Map<Object, Object> userData = repo.getUser(username);
        User user = new User();
        user.setUsername(username);
        user.setEmail((String) userData.get("email"));
        return user;
    }

    // update user profile
    public void updateUserProfile(String username, String newUsername, String newEmail, String newPassword) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Current username cannot be null or empty");
        }

        // Check if the user exists
        if (!repo.userExists(username)) {
            throw new IllegalStateException("User does not exist");
        }

        Map<String, String> updates = new HashMap<>();
        if (newUsername != null && !newUsername.trim().isEmpty() && !newUsername.equals(username)) {
            if (repo.userExists(newUsername)) {
                throw new IllegalStateException("Username already taken");
            }
            Map<Object, Object> currentUserData = repo.getUser(username);
            Map<String, String> updatedData = new HashMap<>();
            currentUserData.forEach((key, value) -> updatedData.put(key.toString(), value.toString()));
            repo.saveUser(newUsername, updatedData);

            repo.deleteUser(username);
            username = newUsername;
        }

        if (newEmail != null && !newEmail.trim().isEmpty()) {
            updates.put("email", newEmail);
        }

        if (newPassword != null && !newPassword.trim().isEmpty()) {
            updates.put("password", newPassword);
        }

        if (!updates.isEmpty()) {
            repo.updateUser(username, updates);
        }
    }

    public void deleteUser(String username, String password) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be null or empty");
        }

        Map<Object, Object> userData = repo.getUser(username);
        if (userData == null || userData.isEmpty()) {
            throw new NoSuchElementException("User not found");
        }
        String storedPassword = (String) userData.get("password");
        if (!storedPassword.equals(password)) {
            throw new IllegalArgumentException("Incorrect password");
        }
        repo.deleteUser(username);
    }
}
