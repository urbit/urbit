(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({"/Users/galen/Documents/Projects/urbit.tlon/chat/pub/src/radio/main.coffee":[function(require,module,exports){
$(function() {
  return window.urb.subscribe({
    appl: "radio",
    path: "/"
  }, function(err, res) {
    console.log('updates');
    return console.log(arguments);
  });
});



},{}]},{},["/Users/galen/Documents/Projects/urbit.tlon/chat/pub/src/radio/main.coffee"]);
