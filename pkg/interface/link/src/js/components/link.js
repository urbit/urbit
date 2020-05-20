import React, { Component } from 'react';
import { LinksTabBar } from './lib/links-tabbar';
import { LinkPreview } from './lib/link-detail-preview';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { api } from '../api';
import { Link } from 'react-router-dom';
import { Comments } from './lib/comments';
import { Spinner } from './lib/icons/icon-spinner';
import { LoadingScreen } from './loading';
import { makeRoutePath, getContactDetails } from '../lib/util';
import CommentItem from './lib/comment-item';

export class LinkDetail extends Component {
  constructor(props) {
    super(props);
    this.state = {
      comment: '',
      data: props.data,
      commentFocus: false,
      pending: new Set(),
      disabled: false
    };

    this.setComment = this.setComment.bind(this);
  }

  updateData(submission) {
    this.setState({
      data: submission
    });
  }

  componentDidMount() {
    // if we have no preloaded data, and we aren't expecting it, get it
    if (!this.state.data.title) {
      api.getSubmission(
        this.props.resourcePath, this.props.url, this.updateData.bind(this)
      );
    }
  }

  componentDidUpdate(prevProps) {
    if (this.props.url !== prevProps.url) {
      this.updateData(this.props.data);
    }
    if (prevProps.comments && prevProps.comments['0'] &&
      this.props.comments && this.props.comments['0']) {
        const prevFirstComment = prevProps.comments['0'][0];
        const thisFirstComment = this.props.comments['0'][0];
        if ((prevFirstComment && prevFirstComment.udon) &&
          (thisFirstComment && thisFirstComment.udon)) {
          if (this.state.pending.has(thisFirstComment.udon)) {
            const pending = this.state.pending;
            pending.delete(thisFirstComment.udon);
            this.setState({
              pending: pending
            });
          }
        }
    }
  }

  onClickPost() {
    const url = this.props.url || '';

    const pending = this.state.pending;
    pending.add(this.state.comment);
    this.setState({ pending: pending, disabled: true  });

    api.postComment(
      this.props.resourcePath,
      url,
      this.state.comment
    ).then(() => {
      this.setState({ comment: '', disabled: false });
    });
  }

  setComment(event) {
    this.setState({ comment: event.target.value });
  }

  render() {
    const props = this.props;

    const data = this.state.data || props.data;

    if (!data.ship) {
      return <LoadingScreen />;
    }

    const ship = data.ship || 'zod';
    const title = data.title || '';
    const url = data.url || '';

    const commentCount = props.comments
      ? props.comments.totalItems
      : data.commentCount || 0;

    const comments = commentCount + ' comment' + (commentCount === 1 ? '' : 's');

    const { nickname } = getContactDetails(props.contacts[ship]);

    const activeClasses = this.state.comment
      ? 'black white-d pointer'
      : 'gray2 b--gray2';

    const focus = (this.state.commentFocus)
      ? 'b--black b--white-d'
      : 'b--gray4 b--gray2-d';

    const our = getContactDetails(props.contacts[window.ship]);

    const pendingArray = Array.from(this.state.pending).map((com, i) => {
      return(
        <CommentItem
          key={i}
          color={our.color}
          nickname={our.nickname}
          ship={window.ship}
          pending={true}
          content={com}
          member={our.member}
          time={new Date().getTime()}
        />
      );
    });

    return (
      <div className="h-100 w-100 overflow-hidden flex flex-column">
        <div
          className={'pl4 pt2 flex relative overflow-x-scroll ' +
      'overflow-x-auto-l overflow-x-auto-xl flex-shrink-0 ' +
      'bb bn-m bn-l bn-xl b--gray4'}
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
          />
          <Link
            className="dib f9 fw4 pt2 gray2 lh-solid"
            to={makeRoutePath(props.resourcePath, props.popout, props.page)}
          >
            {`<- ${props.resource.metadata.title}`}
          </Link>
          <LinksTabBar {...props} popout={props.popout} resourcePath={props.resourcePath} />
        </div>
        <div className="w-100 mt2 flex justify-center overflow-y-scroll ph4 pb4">
          <div className="w-100 mw7">
            <LinkPreview
              title={title}
              url={url}
              comments={comments}
              nickname={nickname}
              ship={ship}
              resourcePath={props.resourcePath}
              page={props.page}
              linkIndex={props.linkIndex}
              time={this.state.data.time}
            />
            <div className="relative">
              <div className={'relative ba br1 mt6 mb6 ' + focus}>
                <textarea
                  className="w-100 bg-gray0-d white-d f8 pa2 pr8"
                  style={{
                    resize: 'none',
                    height: 75
                  }}
                  placeholder="Leave a comment on this link"
                  onChange={this.setComment}
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
                  value={this.state.comment}
                />
                <button
                  className={
                    'f8 bg-gray0-d ml2 absolute ' + activeClasses
                  }
                  disabled={!this.state.comment || this.state.disabled}
                  onClick={this.onClickPost.bind(this)}
                  style={{
                    bottom: 12,
                    right: 8
                  }}
                >
                  Post
                </button>
              </div>
              <Spinner awaiting={this.state.disabled} classes="absolute pt5 right-0" text="Posting comment..." />
              {pendingArray}
            </div>
            <Comments
              resourcePath={props.resourcePath}
              key={props.resourcePath + props.commentPage}
              comments={props.comments}
              commentPage={props.commentPage}
              contacts={props.contacts}
              popout={props.popout}
              url={props.url}
              linkPage={props.page}
              linkIndex={props.linkIndex}
            />
          </div>
        </div>
      </div>
    );
  }
}

export default LinkDetail;
