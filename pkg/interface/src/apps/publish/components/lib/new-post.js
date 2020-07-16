import React, { Component } from 'react';
import { SidebarSwitcher } from '../../../../components/SidebarSwitch';
import { Spinner } from '../../../../components/Spinner';
import { Link } from 'react-router-dom';
import { Controlled as CodeMirror } from 'react-codemirror2';
import { dateToDa, stringToSymbol } from '../../../../lib/util';

import 'codemirror/mode/markdown/markdown';

export class NewPost extends Component {
  constructor(props) {
    super(props);
    this.state = {
      body: '',
      title: '',
      submit: false,
      awaiting: null,
      disabled: false
    };

    this.postSubmit = this.postSubmit.bind(this);
    this.titleChange = this.titleChange.bind(this);
    this.bodyChange = this.bodyChange.bind(this);
  }

  postSubmit() {
    const { state } = this;
    if (state.submit && !state.disabled) {
      const newNote = {
        'new-note': {
          who: this.props.ship.slice(1),
          book: this.props.book,
          note: stringToSymbol(this.state.title),
          title: this.state.title,
          body: this.state.body
        }
      };

      this.setState({ disabled: true });
      this.props.api.publish.publishAction(newNote).then(() => {
        this.setState({ awaiting: newNote['new-note'].note });
      }).catch((err) => {
        if (err.includes('note already exists')) {
          const timestamp = Math.floor(Date.now() / 1000);
          newNote['new-note'].note += '-' + timestamp;
          this.setState({ awaiting: newNote['new-note'].note });
          this.props.api.publish.publishAction(newNote);
        } else {
          this.setState({ disabled: false, awaiting: null });
        }
      });
    }
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate(prevProps) {
    if (prevProps && prevProps.api !== this.props.api) {
      this.props.api.publish.fetchNotebook(this.props.ship, this.props.book);
    }

    const notebook = this.props.notebooks[this.props.ship][this.props.book];
    if (notebook.notes[this.state.awaiting]) {
      this.setState({ disabled: false, awaiting: null });
      const popout = (this.props.popout) ? 'popout/' : '';
      const redirect =
     `/~publish/${popout}note/${this.props.ship}/${this.props.book}/${this.state.awaiting}`;
      this.props.history.push(redirect);
    }
  }

  titleChange(evt) {
    const submit = !(evt.target.value === '' || this.state.body === '');
    this.setState({ title: evt.target.value, submit: submit });
  }

  bodyChange(editor, data, value) {
    const submit = !(value === '' || this.state.title === '');
    this.setState({ body: value, submit: submit });
  }

  render() {
    const { props, state } = this;

    const notebook = props.notebooks[props.ship][props.book];

    const options = {
      mode: 'markdown',
      theme: 'tlon',
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: null,
      cursorHeight: 0.85
    };

    const date = dateToDa(new Date()).slice(1, -10);

    const submitStyle = ((!state.disabled && state.submit) && (state.awaiting === null))
      ? { color: '#2AA779', cursor: 'pointer' }
      : { color: '#B1B2B3', cursor: 'auto' };

    const hrefIndex = props.location.pathname.indexOf('/notebook/');
    const publishsubStr = props.location.pathname.substr(hrefIndex);
    const popoutHref = `/~publish/popout${publishsubStr}`;

    const hiddenOnPopout = (props.popout)
      ? '' : 'dib-m dib-l dib-xl';

    const newIndex = props.location.pathname.indexOf('/new');
    const backHref = props.location.pathname.slice(0, newIndex);
    return (
      <div className='f9 h-100 relative publish'>
        <div className='w-100 dn-m dn-l dn-xl inter pt4 pb4 f9 pl4'>
          <Link to={backHref}>{'<- Back'}</Link>
        </div>
          <SidebarSwitcher
            popout={props.popout}
            sidebarShown={props.sidebarShown}
            api={this.props.api}
            classes="absolute top-1 pl4"
          />
        <div className='w-100 tl pv4 flex justify-center'>
          <button
            className={'bg-transparent v-mid w-100 w-90-l w-80-m mw6 tl h1 pl4'}
            disabled={(!state.submit && state.disabled) || (state.awaiting !== null)}
            style={submitStyle}
            onClick={this.postSubmit}
          >
            Publish To {notebook.title}
          </button>
          <Link
            className={'dn absolute right-1 top-1 ' + hiddenOnPopout}
            to={popoutHref}
            target='_blank'
          >
            <img src='/~landscape/img/popout.png' height={16} width={16} />
          </Link>
        </div>
        <div className='mw6 center'>
          <div className='pa4'>
            <input
              autoFocus
              type='text'
              className='bg-transparent white-d w-100 pb2'
              onChange={this.titleChange}
              placeholder='New Post'
            />

            <div className='gray2'>{date}</div>
          </div>

          <div className='NewPost'>
            <CodeMirror
              value={state.body}
              options={options}
              onBeforeChange={(e, d, v) => this.bodyChange(e, d, v)}
              onChange={(editor, data, value) => {}}
            />
            <Spinner
              text='Creating post...'
              awaiting={this.state.disabled}
              classes='absolute bottom-1 right-1 ba b--gray1-d pa2'
            />
          </div>
        </div>
      </div>
    );
  }
}

export default NewPost;
