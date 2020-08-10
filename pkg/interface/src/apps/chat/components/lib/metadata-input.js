
export class MetadataInput extends Component {

  constructor(props) {
    super(props);

    this.state = {
      value: props.initialValue
    };
    
    this.inputRef = React.createRef();
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ value: props.initialValue }, () => {
        if (this.inputRef.current) {
          this.inputRef.current.value = props.initialValue;
        }
      });
    }
  }

  render() {
    const {
      isDisabled,
      changeValue,
      title,
      description
    } = this.props;

    <div className={'w-100 pb6 fl mt3 ' + ((isOwner) ? '' : 'o-30')}>
      <p className="f8 mt3 lh-copy">{title}</p>
      <p className="f9 gray2 db mb4">{description}</p>
      <div className="relative w-100 flex" style={{ maxWidth: '29rem' }}>
        <input
          ref={this.inputRef}
          className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
          'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
          value={state.value}
          disabled={isDisabled}
          onBlur={() => {
            if (!isDisabled) {
              changeValue(state.value);
            }
          }}
        />
      </div>
    </div>
  }
}
