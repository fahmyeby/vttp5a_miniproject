package com.example.vttp5a_miniproject.repo;

import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class RedisRepo {
    @Autowired
    @Qualifier("redisTemplate")
    private RedisTemplate<String, String> template;

    // prefixes for redis keys
    private static final String user = "user:";
    private static final String watchlist = "watchlist:";
    private static final String rating = "rating:";
    private static final String review = "review:";

    // user ops

    // save user -- HSET user:john_doe "John Doe" email "john@email.com" ...
    public void saveUser(String username, Map<String, String> userData) {
        String userKey = user + username;
        template.opsForHash().putAll(userKey, userData);
    }

    // get user -- HGETALL user:john_doe
    public Map<Object, Object> getUser(String username) {
        String userKey = user + username;
        return template.opsForHash().entries(userKey);
    }

    // check if user exists -- EXISTS user:john_doe
    public boolean userExists(String username) {
        String userKey = user + username;
        return Boolean.TRUE.equals(template.hasKey(userKey));
    }

    // update user -- HSET user:john_doe email "new_email@example.com"
    public void updateUser(String username, Map<String, String> updates) {
        if (username == null || updates == null || updates.isEmpty()) {
            throw new IllegalArgumentException("Invalid update parameters");
        }

        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }

        template.opsForHash().putAll(userKey, updates);
    }

    // delete user -- DEL user:john_doe
    public void deleteUser(String username) {
        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }
        template.delete(userKey);
    }

    // watchlist ops

    // save movie to watchlist -- HSET watchlist:john_doe movie_123 "{\"id\":123,\"title\":\"Movie Title\",\"overview\":\"Some overview\",\"releaseDate\":\"2024-01-01\"}"
    public void saveToWatchlist(String username, String movieId, String movieData) {
        template.opsForHash().put(watchlist + username, movieId, movieData);
    }

    // remove from watchlist -- HDEL watchlist:john_doe movie_123
    public void removeFromWatchlist(String username, String movieId) {
        Long removed = template.opsForHash().delete(watchlist + username, movieId);
        if (removed == 0) {
            throw new NoSuchElementException("Movie not found in watchlist");
        }
    }

    // get watchlist -- HGETALL watchlist:john_doe
    public Map<Object, Object> getWatchlist(String username) {
        return template.opsForHash().entries(watchlist + username);
    }

    // rating ops

    // save rating
    public void saveRating(String username, String movieId, String rating) {
        template.opsForHash().put(rating + user, movieId, rating);
    }

    // get rating
    public String getRating(String username, String movieId) {
        String getRating = (String) template.opsForHash().get(rating + username,
                movieId);
        return getRating;
    }

    // get all ratings
    public Map<Object, Object> getAllRatings(String username) {
        return template.opsForHash().entries(rating + username);
    }


    // profile update ops
    // update user profile
    public void updateUserProfile(String username, Map<String, String> updates) {
        if (username == null || updates == null || updates.isEmpty()) {
            throw new IllegalArgumentException("Invalid update parameters");
        }

        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }

        template.opsForHash().putAll(userKey, updates);
    }

    public void renameUser(String oldUsername, String newUsername) {
        if (userExists(newUsername)) {
            throw new IllegalStateException("New username is already taken");
        }
    
        // get user data for old username
        Map<Object, Object> userData = getUser(oldUsername);
        if (userData == null || userData.isEmpty()) {
            throw new IllegalArgumentException("User with username '" + oldUsername + "' does not exist");
        }
    
        // save data with new username
        saveUser(newUsername, (Map<String, String>) (Map<?, ?>) userData);
    
        // remove the old username data
        template.delete(oldUsername);
    }
    

    // update username
    public void updateUsername(String oldUsername, String newUsername) {
        if (oldUsername == null || newUsername == null) {
            throw new IllegalArgumentException("Usernames cannot be null");
        }

        String oldKey = user + oldUsername;
        String newKey = user + newUsername;

        if (Boolean.FALSE.equals(template.hasKey(oldKey))) {
            throw new NoSuchElementException("User not found");
        }
        if (Boolean.TRUE.equals(template.hasKey(newKey))) {
            throw new IllegalStateException("New username already exists");
        }

        // copy user data
        Map<Object, Object> userData = template.opsForHash().entries(oldKey);
        template.opsForHash().putAll(newKey, userData);

        // copy watchlist
        String oldWatchlist = watchlist + oldUsername;
        String newWatchlist = watchlist + newUsername;
        Map<Object, Object> watchlistData = template.opsForHash().entries(oldWatchlist);
        if (!watchlistData.isEmpty()) {
            template.opsForHash().putAll(newWatchlist, watchlistData);
        }

        // delete old data
        template.delete(oldKey);
        template.delete(oldWatchlist);
    }

    //review ops
    // save review -- SET review:john_doe:movie_123 "{\"review\":\"Great movie!\",\"rating\":4.5}"
     public void saveReview(String username, String movieId, String reviewJson) {
        String key = review + username + ":" + movieId;
        template.opsForValue().set(key, reviewJson);
    }

    // get review -- GET review:john_doe:movie_123
    public String getReview(String username, String movieId) {
        String key = review + username + ":" + movieId;
        return template.opsForValue().get(key);
    }

    // get all reviews
    // KEYS review:john_doe:* -- get all review keys for the user
    // GET review:john_doe:movie_123  -- get the review data

    public Map<Object, Object> getUserReviews(String username) {
        String pattern = review + username + ":*";
        Set<String> keys = template.keys(pattern); //get all keys for username
        Map<Object, Object> reviews = new HashMap<>();
        if (keys != null) {
            for (String key : keys) { // iterate
                String value = template.opsForValue().get(key); //get value with key
                if (value != null) {
                    reviews.put(key.split(":")[2], value); //get 3rd part of key as review id
                }
            }
        }
        return reviews;
    }

    public void deleteReview(String username, String movieId) {
        String key = review + username + ":" + movieId;
        template.delete(key);
    }

}
