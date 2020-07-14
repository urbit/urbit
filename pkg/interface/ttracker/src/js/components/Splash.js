import React, { Component } from 'react';
import _ from 'lodash';


export class Splash extends Component {
    constructor(props) {
      super(props);
    }

    render() {
        return(
            <article class="vh-100 dt w-100">
            <div class="dtc v-mid tc ph3 ph4-l">
              <h1 class="f6 f2-m f-subheadline-l fw6 tc">Boston T Tracker</h1>
              <input className type="text" placeholder='Please enter your API key'>

              </input>
            </div>
          </article>
               
        );
        
    }
}