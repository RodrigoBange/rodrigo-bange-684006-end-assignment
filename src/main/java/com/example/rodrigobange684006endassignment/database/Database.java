package com.example.rodrigobange684006endassignment.database;

abstract class Database <T> {
    abstract void add(T entity);
    abstract void remove(T entity);
    abstract void update(T entity);
}
