import React, { Component } from 'react';
import { Sigil } from '../../../../lib/sigil';
import PostEditor from './post-editor';


const URL_REGEX = new RegExp(String(/^((\w+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+)/.source));

export class PostInput extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      inCodeMode: false
    };

    this.submit = this.submit.bind(this);
    this.toggleCode = this.toggleCode.bind(this);
  }

  toggleCode() {
    this.setState({
      inCodeMode: !this.state.inCodeMode
    });
  }

  getLetterType(letter) {
    if (this.isUrl(letter)) {
       return {
        url: letter
      };
    } else {
      return {
        text: letter
      };
    }
  }

  isUrl(string) {
    try {
      return URL_REGEX.test(string);
    } catch (e) {
      return false;
    }
  }

  submit(text) {
    const { props, state } = this;
    if (state.inCodeMode) {
      let post = props.api.createPost([
        {
          code: {
            expression: text,
            output: null
          }
        }
      ], props.parentIndex);

      this.setState({
        inCodeMode: false
      }, () => {
        props.api.addPost(props.resource.ship, props.resource.name, post);
      });
      return;
    }

    let message = this.getLetterType(text);
    let post = props.api.createPost([message], props.parentIndex);
    props.api.addPost(props.resource.ship, props.resource.name, post);

    // perf testing:
    /*let closure = () => {
      let x = 0;
      for (var i = 0; i < 30; i++) {
        x++;
        props.api.addPost(
          props.resource.ship,
          props.resource.name,
          props.api.createPost([{
            text: `${i} - ${Date.now()}`
          }])
        );
      }
      setTimeout(closure, 1000);
    };
    setTimeout(closure, 2000);*/
  }

  render() {
    const { props, state } = this;
    return (
      <div className="pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white bg-gray0-d relative"
      style={{ flexGrow: 1 }}
      >
        <div
          className="fl"
          style={{
            marginTop: 6,
            flexBasis: 24,
            height: 24
          }}
        >
        <Sigil
          ship={window.ship}
          size={24}
          color={`#000`}
          />
        </div>
        <PostEditor
          inCodeMode={state.inCodeMode}
          submit={this.submit}
          placeholder='Post...' />
        <div className="ml2 mr2"
          style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
        </div>
        <div style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
          <img
            style={{ filter: state.inCodeMode && 'invert(100%)', height: '100%', width: '100%' }}
            onClick={this.toggleCode}
            src="/~chat/img/CodeEval.png"
            className="contrast-10-d bg-white bg-none-d ba b--gray1-d br1"
          />
        </div>
      </div>
    );
  }
}
