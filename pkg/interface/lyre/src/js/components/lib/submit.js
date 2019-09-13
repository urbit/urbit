import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Submit extends Component {
  constructor(props) {
    super(props);
    this.submitAction = this.submitAction.bind(this);
  }


  getParentForm(elm) {
    let result = elm;
    while (result.parentNode) {
      result = result.parentNode;
      if (result.tagName === "FORM"){
        return result;
      }
    }
    return false;
  }

  submitAction(evt) {
    evt.preventDefault();
  
    let form = this.getParentForm(this.button);

    if (form) {
      let formDat = new FormData(form);
      let data = {}
      for (var pair of formDat.entries()) {
        data[pair[0]] = pair[1];
      }

      let app = form.getAttribute("app");
      let mark = form.getAttribute("mark");


      this.props.api.action(app, mark, data);
    } else {
      console.error("submit button does not have a form element ancestor");
    }
  }



  render() {
    return (
      <button onClick={this.submitAction}
          ref={(el) => { this.button = el}}>
        <Dom body={this.props.body}/>
      </button>
    );
  }
}
