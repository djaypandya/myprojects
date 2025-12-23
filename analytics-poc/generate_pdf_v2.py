from md2pdf.core import md2pdf
import os

REPORT_FILE = 'storytelling_report.md'
PDF_FILE = 'Fraud_Analytics_Report.pdf'
CSS_FILE = 'style.css'

def convert():
    if not os.path.exists(REPORT_FILE):
        print(f"Error: {REPORT_FILE} not found.")
        return

    # Create a simple CSS for styling
    with open(CSS_FILE, 'w') as f:
        f.write("""
        body { font-family: Helvetica, sans-serif; font-size: 12pt; }
        h1 { color: #2c3e50; border-bottom: 1px solid #ccc; }
        h2 { color: #e67e22; margin-top: 20px; }
        img { max-width: 100%; }
        code { background: #f4f4f4; padding: 2px; }
        """)

    print(f"Converting {REPORT_FILE} to {PDF_FILE}...")
    try:
        md2pdf(PDF_FILE, md_content=None, md_file_path=REPORT_FILE, css_file_path=CSS_FILE, base_url=os.getcwd())
        print(f"Success! PDF saved to {os.path.abspath(PDF_FILE)}")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    convert()
