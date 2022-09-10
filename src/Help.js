import React from 'react';
import Square from './Square';

class Help extends React.Component {

    render() {
        const helpDepth = this.props.optimalHelp ? 5 : 24;
        return (
            <div className='rightPanel'>
                <div className="textLabel">Moves</div>
                <select className="selectBox" name="select" onChange={e => this.props.setHelpDepth(e.target.value)}>
                    {Array.from({ length: helpDepth }, (x, i) => i).map(j =>
                        <option key={"x" + (j + 1)}>{j + 1}</option>
                    )}
                </select>
                <div className="optPanel">
                    <input type="checkbox" className="checkBox" defaultChecked={true} onChange={e => this.props.setHelpMode(e.target.checked)}></input>
                    <div className="textLabel">Optimal</div>
                </div>
                <button
                    className="textBtn"
                    onClick={() => this.props.getHelp()}
                >Help</button>
                {this.props.loadingHelp ?
                    <div className="helpWaitingPanel">Searching...
                        <div className="spinner" />
                    </div>
                    :
                    <div>
                        {this.props.helpCaptures !== 0 &&
                            <div>to capture {this.props.helpCaptures}:</div>
                        }
                        <div className="helpPanel">
                            {this.props.help.map((color, i) =>
                                <Square
                                    value={color}
                                    key={color + i}
                                />)}
                        </div>
                    </div>
                }

            </div>
        );
    }
}

export default Help;