import React, { Component } from 'react';
import { CommentItem } from './comment-item';
import CommentInput from './comment-input';
import { dateToDa } from '../../../../lib/util';
import { Spinner } from '../../../../components/Spinner';

export class Comments extends Component {
  constructor(props) {
    super(props);
    this.state = {
      commentBody: '',
      pending: new Set(),
      awaiting: null,
      editing: null
    };
    this.commentSubmit = this.commentSubmit.bind(this);
    this.commentChange = this.commentChange.bind(this);
  }

  componentDidUpdate(prevProps) {
    const previousComments = prevProps.comments[0] || {};
    const currentComments = this.props.comments[0] || {};
    const previous = Object.keys(previousComments) || [];
    const current = Object.keys(currentComments) || [];
    if ((prevProps.comments && this.props.comments) &&
      (previous !== current)) {
        const pendingSet = this.state.pending;
        Object.keys(currentComments).map((com) => {
          const obj = currentComments[com];
          for (const each of pendingSet.values()) {
            if (obj.content === each['new-comment'].body) {
              pendingSet.delete(each);
              this.setState({ pending: pendingSet });
            }
          }
        });
      }
    }

  commentSubmit(evt) {
   const comment = {
     'new-comment': {
       who: this.props.ship.slice(1),
       book: this.props.book,
       note: this.props.note,
       body: this.state.commentBody
     }
   };

   const pendingState = this.state.pending;
   pendingState.add(comment);
   this.setState({ pending: pendingState });

   this.textArea.value = '';
   this.setState({ commentBody: '', awaiting: 'new' });
    const submit = this.props.api.publish.publishAction(comment);
   submit.then(() => {
     this.setState({ awaiting: null });
    });
   }

  commentChange(evt) {
    this.setState({
      commentBody: evt.target.value
    });
  }

  commentEdit(idx) {
    this.setState({ editing: idx });
  }

  commentEditCancel() {
    this.setState({ editing: null });
  }

  commentUpdate(idx, body) {
    const path = Object.keys(this.props.comments[idx])[0];
    const comment = {
      'edit-comment': {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        body: body,
        comment: path
      }
    };

    this.setState({ awaiting: 'edit' });

    this.props.api.publish
      .publishAction(comment)
      .then(() => {
    this.setState({ awaiting: null, editing: null });
    });
  }

  commentDelete(idx) {
    const path = Object.keys(this.props.comments[idx])[0];
    const comment = {
      'del-comment': {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        comment: path
      }
    };

    this.setState({ awaiting: { kind: 'del', what: idx } });
    this.props.api.publish
      .publishAction(comment)
      .then(() => {
 this.setState({ awaiting: null });
});
  }

  render() {
    if (!this.props.enabled) {
      return null;
    }

    const { editing } = this.state;

    const pendingArray = Array.from(this.state.pending).map((com, i) => {
      const da = dateToDa(new Date());
      const comment = {
        [da]: {
          author: `~${window.ship}`,
          content: com['new-comment'].body,
          'date-created': Math.round(new Date().getTime())
        }
      };
      return (
        <CommentItem
          comment={comment}
          key={i}
          contacts={this.props.contacts}
          pending={true}
        />
      );
    });

    const commentArray = this.props.comments.map((com, i) => {
      return (
        <CommentItem
          comment={com}
          key={i}
          contacts={this.props.contacts}
          onUpdate={u => this.commentUpdate(i, u)}
          onDelete={() => this.commentDelete(i)}
          onEdit={() => this.commentEdit(i)}
          onEditCancel={this.commentEditCancel.bind(this)}
          editing={i === editing}
          disabled={Boolean(this.state.awaiting) || editing}
        />
      );
    });

    const disableComment = ((this.state.commentBody === '') || (Boolean(this.state.awaiting)));
    const commentClass = (disableComment)
      ?  'bg-transparent f9 pa2 br1 ba b--gray2 gray2'
      :  'bg-transparent f9 pa2 br1 ba b--gray2 black white-d pointer';

    const spinnerText =
        this.state.awaiting === 'new'
        ? 'Posting commment...'
        : this.state.awaiting  === 'edit'
        ? 'Updating comment...'
        : 'Deleting comment...';

    return (
      <div>
        <div className="mv8 relative">
          <div>
            <CommentInput style={{ resize:'vertical' }}
              ref={(el) => {
              this.textArea = el;
              }}
              onChange={this.commentChange}
              value={this.state.commentBody}
              disabled={Boolean(this.state.editing)}
              onSubmit={this.commentSubmit}
            >
            </CommentInput>
          </div>
          <button disabled={disableComment}
            onClick={this.commentSubmit}
            className={commentClass}
          >
            Add comment
          </button>
          <Spinner text={spinnerText} awaiting={this.state.awaiting} classes="absolute bottom-0 right-0 pb2" />
        </div>
        {pendingArray}
        {commentArray}
      </div>
    );
  }
}

export default Comments;
