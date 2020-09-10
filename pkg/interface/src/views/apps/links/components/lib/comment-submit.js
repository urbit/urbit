import React, { Component } from 'react';
import { Spinner } from '~/views/components/Spinner';
import { createPost } from '~/logic/api/graph';


export class CommentSubmit extends Component {
  constructor(props) {
    super(props);

    this.state = {
      comment: '',
      commentFocus: false,
      disabled: false
    };
  }

  onClickPost() {
    const parentIndex = this.props.parentIndex || '';
    let post = createPost([
      { text: this.state.comment },
    ], parentIndex);

    this.setState({ disabled: true }, () => {
      this.props.api.graph.addPost(
        `~${this.props.ship}`,
        this.props.name,
        post
      ).then((r) => {
        this.setState({
          disabled: false,
          comment: ''
        });
      });
    });
  }

  setComment(event) {
    this.setState({ comment: event.target.value });
  }

  render() {
    const { state, props } = this;
    const focus = (state.commentFocus)
      ? 'b--black b--white-d'
      : 'b--gray4 b--gray2-d';

    const activeClasses = state.comment
      ? 'black white-d pointer'
      : 'gray2 b--gray2';

    return (
      <div className={'w-100 relative ba br1 mt6 mb6 ' + focus}>
        <textarea
          className="w-100 bg-gray0-d white-d f8 pa2 pr8"
          style={{ resize: 'none', height: 75 }}
          placeholder="Leave a comment on this link"
          onChange={this.setComment.bind(this)}
          onKeyDown={(e) => {
            if (
              (e.getModifierState('Control') || e.metaKey) &&
              e.key === 'Enter'
            ) {
              this.onClickPost();
            }
          }}
          onFocus={() => this.setState({ commentFocus: true })}
          onBlur={() => this.setState({ commentFocus: false })}
          value={state.comment}
        />
        <button
          className={
            'f8 bg-gray0-d ml2 absolute ' + activeClasses
          }
          disabled={state.disabled}
          onClick={this.onClickPost.bind(this)}
          style={{ bottom: 12, right: 8 }}>
          Post
        </button>
      </div>
    );
  }
}

