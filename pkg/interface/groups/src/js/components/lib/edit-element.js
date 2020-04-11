import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';

export class EditElement extends Component {

  constructor(props) {
    super(props);
    this.state = {
      currentValue: ''
    };
  }

  render() {
    const { props, state } = this;
    // TODO: make ref clear function and make it work
    let showDelete = props.defaultValue === "";
    let allowSave = (
      props.defaultValue !== state.currentValue &&
      state.currentValue !== ""
    );

    let inputStyles = (props.resizable)
      ? { resize: "vertical", height: 40, paddingTop: 10 }
      : { resize: "none", height: 40, paddingTop: 10 }

    return (
      <div className="pb4">
        <p className="f9 gray2">{props.title}</p>
          <div className="w-100 flex">
            <textarea
              ref={props.title}
              className={"w-100 ba pl3 white-d bg-gray0-d b--gray4 b--gray2-d " +
              "focus-b--black focus-b--white-d"}
              style={ inputStyles }
              onChange={(e) => {
                let val = (' ' + e.target.value).slice(1);
                this.setState({
                  currentValue: val
                }, () => {
                  props.onChange(val);
                });
              }}
              defaultValue={props.defaultValue} />
            {!!props.showButtons ? (
              <button
                className={
                  "bg-gray0-d f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                  (showDelete ? "dn" : "dib")
                }
                onClick={() => {
                  this.refs[props.title].value = "";
                  props.onDeleteClick();
                }}>
                Delete
              </button>
            ) : null}
          </div>
          {!!props.showButtons ? (
            <button
              className={
                "bg-gray0-d white-d pointer db mv2 f9 ba pa2 pl3 pr3 " +
                (allowSave ? "b--black b--white-d" : "b--gray4 gray4 b--gray2-d gray2-d")
              }
              onClick={() => {
                if (!allowSave) { return; }
                props.onSaveClick();
              }}>
              Save
            </button>
          ) : null}
      </div>
    );
  }
}

