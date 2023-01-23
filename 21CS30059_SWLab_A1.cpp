#include <iostream>
#include <cmath>
#include <vector>
#define EPSILON 0.000001
using namespace std;

struct Polyn {
    int power;
    int coefficient;
    Polyn* next;
};

void addremoveterm( struct Polyn** head, int nodePower, int nodeCoefficient)  
{  
    if(nodeCoefficient==0)
    return;
   struct Polyn* newNode1 = new Polyn;  
   newNode1 -> power = nodePower;
   newNode1 -> coefficient = nodeCoefficient;
   if(*head==NULL|| (*head)->power< newNode1->power)
   {
        newNode1 -> next = (*head);  
        (*head) = newNode1;
   }
   else if((*head)->power==newNode1->power)
   {
        (*head)->coefficient+=newNode1->coefficient;
        return;
   }
   else{
        struct Polyn* current = *head;
        if(current->next!=NULL&&current->next->power==newNode1->power)
            {
                current->next->coefficient+=newNode1->coefficient;
                return;
            }
        while (current->next != NULL && current->next->power > newNode1->power) {
            if(current->next->power==newNode1->power)
            {
                current->next->coefficient+=newNode1->coefficient;
                return;
            }
            current = current->next;
        }
        newNode1->next = current->next;
        current->next = newNode1;
    }
}
void addremoveterm(Polyn* &head, int a, int b, double eps)
{
    Polyn* temp = head;
    Polyn* prev = nullptr;

    while (temp != nullptr) {
        if (abs(temp->coefficient) < eps) {
            if (prev == nullptr) {
                head = temp->next;
            } else {
                prev->next = temp->next;
            }
            delete temp;
        }
        prev = temp;
        temp = temp->next;
    }
}
void addremoveterm(Polyn* &head, int degree) {
    Polyn* temp = head;
    Polyn* prev = nullptr;

    while (temp != nullptr) {
        if (temp->power == degree) {
            if (prev == nullptr) {
                head = temp->next;
            } else {
                prev->next = temp->next;
            }
            delete temp;
            return;
        }
        prev = temp;
        temp = temp->next;
    }
}
Polyn* addPolynomials(struct Polyn* p1,struct Polyn* p2){
    Polyn *result = nullptr, *temp;
    Polyn *p1_current = p1, *p2_current = p2;

    while (p1_current != nullptr && p2_current != nullptr) {
        if (p1_current->power > p2_current->power) {
            addremoveterm(&result,p1_current->power,p1_current->coefficient);
            p1_current=p1_current->next;
        } else if (p1_current->power < p2_current->power) {
            addremoveterm(&result,p2_current->power,p2_current->coefficient);
            p2_current=p2_current->next;
        } else {
            int coef = p1_current->coefficient + p2_current->coefficient;
            if (coef != 0) {
                addremoveterm(&result, p1_current->power, coef);
            }
            p1_current = p1_current->next;
            p2_current = p2_current->next;
        }
    }

    while (p1_current != nullptr) {
        addremoveterm(&result,p1_current->power,p1_current->coefficient);
        p1_current = p1_current->next;
    }

    while (p2_current != nullptr) {
        addremoveterm(&result,p2_current->power,p2_current->coefficient);
        p2_current = p2_current->next;
    }
    return result;
}

Polyn* differentiate(struct Polyn* start) {
    Polyn* result = NULL;
    Polyn* term;
    Polyn* tail=NULL;
    while (start!=nullptr) {
        if(start->power>0)
        {
            term=new Polyn;
            term->coefficient = start->coefficient * start->power;
            term->power = start->power - 1;
        }
        start = start->next;
        if (result == NULL) {
            result = term;
            tail = term;
        } else {
            tail->next = term;
            tail = term;
           
        }
    }
     tail->next=nullptr;
    return result;
}
double evaluate(struct Polyn* p, double x)
{
    double sum=0;
    while(p!=NULL)
    { 
        sum+=(p->coefficient)*(pow(x,p->power));
        p=p->next;
    }
    return sum;
}

double zeronewton(Polyn* p,double x,double epsilon=EPSILON)
{
    displayList(differentiate(p));
    double fraction=evaluate(p,x)/evaluate((differentiate(p)),x);
    int count=0;
    while(fraction>=epsilon&&count<=1000)
    {
        count++;
        x=x-fraction;
        fraction=evaluate(p,x)/evaluate((differentiate(p)),x);\
    }
    return x;
}

void displayList(struct Polyn *node )  
{  
    
    if ( node== NULL)  
    cout<<"Empty Polynomial";   
    while ( node != NULL )  
    {  
      if(node->power>0&&node->coefficient==-1)
      cout<<"-";
      else if(!(node->coefficient==1&&node->power>0))
      cout << node -> coefficient;
      if(node->power>0)
      cout<< "x^" << node->power << ""; 
      if(node->next==NULL)
      cout<<"\n";
      else if(node->next->coefficient>=0)
      cout<<"+"; 
      node = node -> next;  
   }  
}  

int main() {
    
    vector<Polyn*> v;
    int choice,n,i,pow,coeff,m;double f,eps;
    while(true)
    {
        cout<<"1. Add a new polynomial\n";
        cout<<"2. Add terms to an existing polynomial\n";
        cout<<"3. Remove terms from an existing polynomial\n";
        cout<<"4. Remove all terms with absolute coefficient values less than a given value epsilon\n";
        cout<<"5. Remove an existing polynomial\n";
        cout<<"6. Find root of an exisiting polynomial using Newton's method\n";
        cout<<"7. Find the derivative of an existing polynomial\n";
        cout<<"8. Add two polynomials\n";
        cout<<"9. Evaluate a polynomial at a given value of x\n";
        cout<<"10. Display all polynmoials\n";
        cout<<"Enter your choice: ";
        cin>>choice;
        switch(choice)
        {
            case 1:
            {
                int pow,coeff;
                Polyn* p=nullptr;
                v.push_back(p);
                while(true)
                {
                    cout<<"Enter degree and coefficient of term to be added to the polynomial [Enter (0,0) to exit]: ";
                    cin>>pow>>coeff;
                    if(pow==0&&coeff==0)
                    break;
                    addremoveterm(&v[v.size()-1],pow,coeff);
                }
            }
            break;
            case 2:
                cout<<"Enter the number (index number) of polynomial in which you want to add a term: ";
                cin>>n;
                cout<<"Enter the power and coefficient of the term to be added: ";
                cin>>pow>>coeff;
                addremoveterm(&v[n-1],pow,coeff);
            break;
            case 3:
            {
                int pow;
                cout<<"Enter the number (index number) of polynomial from which you want to remove a term: ";
                cin>>n;
                cout<<"Enter the power of the term to be removed: ";
                cin>>pow;
                addremoveterm(v[n-1],pow);
            }
            break;
            case 4:
                cout<<"Enter the number (index number) of polynomial from which you want to remove terms: ";
                cin>>n;
                cout<<"Enter the value of epsilon: ";
                cin>>eps;
                addremoveterm(v[n-1],0,0,eps);
                cout<<"Modified polynomial: ";
                displayList(v[n-1]);
            break;
            case 5:
                cout<<"Enter the number (index number) of polynomial which you want to remove: ";
                cin>>n;
                v.erase(v.begin()+n-1);
                cout<<"Polynomial "<<n<<" removed. Polynomial index numbers updated acccordingly.";
            break;
            case 6:
                cout<<"Enter the number (index number) of polynomial whose root you want to find: ";
                cin>>n;
                cout<<"Enter an initial guess for the root and value of epsilon: ";
                cin>>f>>eps;
                cout<<zeronewton(v[n-1],f,eps)<<"\n";
            break;
            case 7:
            {
                cout<<"Enter the number (index number) of the polynomial whose derivative has to be calculated: ";
                cin>>n;
                Polyn* deri=differentiate(v[n-1]);
                displayList(deri);
            }
            break;
            case 8:
            {
                cout<<"Enter the index numbers of the two polynomials to be added: ";
                cin>>n>>m;
                Polyn* p3=addPolynomials(v[n-1],v[m-1]);
                cout<<"\nAdded Polynomial: ";
                displayList(p3);
                cout<<"\n";
            }
            break;
            case 9:
                cout<<"Enter the number (index number) of the polynomial to be evaluated: ";
                cin>>n;
                cout<<"Enter the value of x at which polynomial is to be evaluated: ";
                cin>>f;
                cout<<evaluate(v[n-1],f);
            break;
            case 10:
                cout<<"\n";
                for(i=0;i<v.size();i++)
                {
                    cout<<"Polynomial "<<(i+1)<<": ";
                    displayList(v[i]);
                }
                cout<<"\n";
            break;
        }
        if(choice>10||choice<1)
        {
            cout<<"Invalid choice";
            break;
        }
    }
    
}