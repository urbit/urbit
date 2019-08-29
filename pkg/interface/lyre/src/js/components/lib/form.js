import React, { Component } from 'react';
import classnames from 'classnames';
import { FormDom } from '/components/lib/form-dom';

export class Form extends Component {
  constructor(props) {
    super(props);
    this.formSubmit = this.formSubmit.bind(this);
  }

  formSubmit(evt) {
    let fd = new FormData(document.getElementById('myForm'));

    let data = {};

    for (var pair of fd.entries()) {
      data[pair[0]] = pair[1];
    }

    this.props.api.action(this.props.app, this.props.mark, data);

    evt.preventDefault();
  }

  render() {
    console.log("form props", this.props);
    return (
      <form id="myForm">
        <FormDom body={this.props.body}
          action={this.formSubmit.bind(this)}/>
      </form>
    );
  }
}
