import React, { Component } from 'react';

const Spinner = ({
  classes = '',
  text = '',
  awaiting = false
}) => awaiting ? (
  <div className={classes + ' z-2 bg-white bg-gray0-d white-d flex'}>
    <img className="invert-d spin-active v-mid"
      src="/~landscape/img/Spinner.png"
      width={16}
      height={16}
    />
    <p className="dib f9 ml2 v-mid inter">{text}</p>
  </div>
) : null;

export { Spinner as default, Spinner };